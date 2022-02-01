/*
** 2015-08-12
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
******************************************************************************
**
** This SQLite extension implements JSON functions.  The interface is
** modeled after MySQL JSON functions:
**
**     https://dev.mysql.com/doc/refman/5.7/en/json.html
**
** For the time being, all JSON is stored as pure text.  (We might add
** a JSONB type in the future which stores a binary encoding of JSON in
** a BLOB, but there is no support for JSONB in the current implementation.
** This implementation parses JSON text at 250 MB/s, so it is hard to see
** how JSONB might improve on that.)
*/
#if !defined(SQLITE_CORE) || defined(SQLITE_ENABLE_JSON1)
#if !defined(SQLITEINT_H)
#include "sqlite3ext.h"
#endif
SQLITE_EXTENSION_INIT1

/* If compiling this extension separately (why would anybody do that when
** it is built into the amalgamation?) we must set NDEBUG if SQLITE_DEBUG
** is not defined *before* including <assert.h>, in order to disable asserts().
*/
#if !defined(SQLITE_AMALGAMATION) && !defined(SQLITE_DEBUG)
#  define NDEBUG 1
#endif

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#ifdef __cplusplus
#include <regex>
#include <string>
#include <sstream>
#include <iomanip>
#endif

/* Mark a function parameter as unused, to suppress nuisance compiler
** warnings. */
#ifndef UNUSED_PARAM
# define UNUSED_PARAM(X)  (void)(X)
#endif

#ifndef LARGEST_INT64
# define LARGEST_INT64  (0xffffffff|(((sqlite3_int64)0x7fffffff)<<32))
# define SMALLEST_INT64 (((sqlite3_int64)-1) - LARGEST_INT64)
#endif

#ifndef deliberate_fall_through
# define deliberate_fall_through
#endif

/*
** Versions of isspace(), isalnum() and isdigit() to which it is safe
** to pass signed char values.
*/
#ifdef sqlite3Isdigit
   /* Use the SQLite core versions if this routine is part of the
   ** SQLite amalgamation */
#  define safe_isdigit(x)  sqlite3Isdigit(x)
#  define safe_isalnum(x)  sqlite3Isalnum(x)
#  define safe_isxdigit(x) sqlite3Isxdigit(x)
#else
   /* Use the standard library for separate compilation */
#include <ctype.h>  /* amalgamator: keep */
#  define safe_isdigit(x)  isdigit((unsigned char)(x))
#  define safe_isalnum(x)  isalnum((unsigned char)(x))
#  define safe_isxdigit(x) isxdigit((unsigned char)(x))
#endif

/*
** Growing our own isspace() routine this way is twice as fast as
** the library isspace() function, resulting in a 7% overall performance
** increase for the parser.  (Ubuntu14.10 gcc 4.8.4 x64 with -Os).
*/
static const char jsonIsSpace[] = {
  0, 0, 0, 0, 0, 0, 0, 0,     0, 1, 1, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
};
#define safe_isspace(x) (jsonIsSpace[(unsigned char)x])

#ifndef SQLITE_AMALGAMATION
  /* Unsigned integer types.  These are already defined in the sqliteInt.h,
  ** but the definitions need to be repeated for separate compilation. */
  typedef sqlite3_uint64 u64;
  typedef unsigned int u32;
  typedef unsigned short int u16;
  typedef unsigned char u8;
# if defined(SQLITE_COVERAGE_TEST) || defined(SQLITE_MUTATION_TEST)
#   define SQLITE_OMIT_AUXILIARY_SAFETY_CHECKS 1
# endif
# if defined(SQLITE_OMIT_AUXILIARY_SAFETY_CHECKS)
#   define ALWAYS(X)      (1)
#   define NEVER(X)       (0)
# elif !defined(NDEBUG)
#   define ALWAYS(X)      ((X)?1:(assert(0),0))
#   define NEVER(X)       ((X)?(assert(0),1):0)
# else
#   define ALWAYS(X)      (X)
#   define NEVER(X)       (X)
# endif
# define testcase(X)
#endif
#if !defined(SQLITE_DEBUG) && !defined(SQLITE_COVERAGE_TEST)
#  define VVA(X)
#else
#  define VVA(X) X
#endif

/*
** Some of the testcase() macros in this file are problematic for gcov
** in that they generate false-miss errors randomly.  This is a gcov problem,
** not a problem in this case.  But to work around it, we disable the
** problematic test cases for production builds.
*/
#define json_testcase(X)

/* Objects */
typedef struct JsonString JsonString;
typedef struct JsonNode JsonNode;
typedef struct JsonParse JsonParse;

/* An instance of this object represents a JSON string
** under construction.  Really, this is a generic string accumulator
** that can be and is used to create strings other than JSON.
*/
struct JsonString {
  sqlite3_context *pCtx;   /* Function context - put error messages here */
  char *zBuf;              /* Append JSON content here */
  u64 nAlloc;              /* Bytes of storage available in zBuf[] */
  u64 nUsed;               /* Bytes of zBuf[] currently used */
  u8 bStatic;              /* True if zBuf is static space */
  u8 bErr;                 /* True if an error has been encountered */
  char zSpace[100];        /* Initial static space */
};

/* JSON type values
*/
#define JSON_NULL     0
#define JSON_TRUE     1
#define JSON_FALSE    2
#define JSON_INT      3
#define JSON_REAL     4
#define JSON_STRING   5
#define JSON_ARRAY    6
#define JSON_OBJECT   7

/* The "subtype" set for JSON values */
#define JSON_SUBTYPE  74    /* Ascii for "J" */

/*
** Names of the various JSON types:
*/
static const char * const jsonType[] = {
  "null", "true", "false", "integer", "real", "text", "array", "object"
};

/* Bit values for the JsonNode.jnFlag field
*/
#define JNODE_RAW     0x01         /* Content is raw, not JSON encoded */
#define JNODE_ESCAPE  0x02         /* Content is text with \ escapes */
#define JNODE_REMOVE  0x04         /* Do not output */
#define JNODE_REPLACE 0x08         /* Replace with JsonNode.u.iReplace */
#define JNODE_PATCH   0x10         /* Patch with JsonNode.u.pPatch */
#define JNODE_APPEND  0x20         /* More ARRAY/OBJECT entries at u.iAppend */
#define JNODE_LABEL   0x40         /* Is a label of an object */


/* A single node of parsed JSON
*/
struct JsonNode {
  u8 eType;              /* One of the JSON_ type values */
  u8 jnFlags;            /* JNODE flags */
  u8 eU;                 /* Which union element to use */
  u32 n;                 /* Bytes of content, or number of sub-nodes */
  union {
    const char *zJContent; /* 1: Content for INT, REAL, and STRING */
    u32 iAppend;           /* 2: More terms for ARRAY and OBJECT */
    u32 iKey;              /* 3: Key for ARRAY objects in json_tree() */
    u32 iReplace;          /* 4: Replacement content for JNODE_REPLACE */
    JsonNode *pPatch;      /* 5: Node chain of patch for JNODE_PATCH */
  } u;
};

/* A completely parsed JSON string
*/
struct JsonParse {
  u32 nNode;         /* Number of slots of aNode[] used */
  u32 nAlloc;        /* Number of slots of aNode[] allocated */
  JsonNode *aNode;   /* Array of nodes containing the parse */
  const char *zJson; /* Original JSON string */
  u32 *aUp;          /* Index of parent of each node */
  u8 oom;            /* Set to true if out of memory */
  u8 nErr;           /* Number of errors seen */
  u16 iDepth;        /* Nesting depth */
  int nJson;         /* Length of the zJson string in bytes */
  u32 iHold;         /* Replace cache line with the lowest iHold value */
};

/*
** Maximum nesting depth of JSON for this implementation.
**
** This limit is needed to avoid a stack overflow in the recursive
** descent parser.  A depth of 2000 is far deeper than any sane JSON
** should go.
*/
#define JSON_MAX_DEPTH  2000

/**************************************************************************
** Utility routines for dealing with JsonString objects
**************************************************************************/


/**************************************************************************
** Utility routines for dealing with JsonNode and JsonParse objects
**************************************************************************/


/*
** Translate a single byte of Hex into an integer.
** This routine only works if h really is a valid hexadecimal
** character:  0..9a..fA..F
*/
static u8 jsonHexToInt(int h){
  assert( (h>='0' && h<='9') ||  (h>='a' && h<='f') ||  (h>='A' && h<='F') );
#ifdef SQLITE_EBCDIC
  h += 9*(1&~(h>>4));
#else
  h += 9*(1&(h>>6));
#endif
  return (u8)(h & 0xf);
}

/*
** Convert a 4-byte hex string into an integer
*/
static u32 jsonHexToInt4(const char *z){
  u32 v;
  assert( safe_isxdigit(z[0]) );
  assert( safe_isxdigit(z[1]) );
  assert( safe_isxdigit(z[2]) );
  assert( safe_isxdigit(z[3]) );
  v = (jsonHexToInt(z[0])<<12)
    + (jsonHexToInt(z[1])<<8)
    + (jsonHexToInt(z[2])<<4)
    + jsonHexToInt(z[3]);
  return v;
}

/*
** A macro to hint to the compiler that a function should not be
** inlined.
*/
#if defined(__GNUC__)
#  define JSON_NOINLINE  __attribute__((noinline))
#elif defined(_MSC_VER) && _MSC_VER>=1310
#  define JSON_NOINLINE  __declspec(noinline)
#else
#  define JSON_NOINLINE
#endif


/*
** Return true if z[] begins with 4 (or more) hexadecimal digits
*/
static int jsonIs4Hex(const char *z){
  int i;
  for(i=0; i<4; i++) if( !safe_isxdigit(z[i]) ) return 0;
  return 1;
}

/*
** Magic number used for the JSON parse cache in sqlite3_get_auxdata()
*/
#define JSON_CACHE_ID  (-429938)  /* First cache entry */
#define JSON_CACHE_SZ  4          /* Max number of cache entries */

/*
** Report the wrong number of arguments for json_insert(), json_replace()
** or json_set().
*/
static void jsonWrongNumArgs(
  sqlite3_context *pCtx,
  const char *zFuncName
){
  char *zMsg = sqlite3_mprintf("json_%s() needs an odd number of arguments",
                               zFuncName);
  sqlite3_result_error(pCtx, zMsg, -1);
  sqlite3_free(zMsg);     
}


/****************************************************************************
** SQL functions used for testing and debugging
****************************************************************************/


/****************************************************************************
** Scalar SQL function implementations
****************************************************************************/

static void float32NullFunc(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  assert( argc==1 );
  UNUSED_PARAM(argc);
  switch( sqlite3_value_type(argv[0]) ){
    case SQLITE_INTEGER: {
      sqlite3_int64 iVal = sqlite3_value_int64(argv[0]);
      size_t n = sizeof(float);
      float f = 0;
      char rVal[sizeof(float)];
      switch(iVal) {
        default: {
          f = (float)iVal;
          break;
        }
        case 1: {
          f = 0.0 / 0.0;
          break;
        }
        case 2: {
          f = NAN;//using the macro in math.h
          break;
        }
        case 3: {
          f = nanf("");
          break;
        }
      }
      memcpy(rVal, &f, n);
      sqlite3_result_blob(context, rVal, n, SQLITE_TRANSIENT);
      break;
    }
    default: {
      sqlite3_result_null(context);
      break;
    }
  }
}


/*
** Bit values for the flags passed into jsonExtractFunc() or
** jsonSetFunc() via the user-data value.
*/
#define JSON_NULLERR   0x01        /* Return NULL if input is not JSON */
#define JSON_ABPATH    0x02        /* Allow abbreviated JSON path specs */
#define JSON_ISSET     0x04        /* json_set(), not json_insert() */


/****************************************************************************
** Aggregate SQL function implementations
****************************************************************************/




#ifndef SQLITE_OMIT_VIRTUALTABLE
/****************************************************************************
** The json_each virtual table
****************************************************************************/

/* An instance of the CSV virtual table */
typedef struct CsvTable {
  sqlite3_vtab base;              /* Base class.  Must be first */
  long iStart;                    /* Offset to start of data in zFilename */
  int nCol;                       /* Number of columns in the CSV file */
  unsigned int tstFlags;          /* Bit values used for testing */
} CsvTable;

typedef struct JsonEachCursor JsonEachCursor;
struct JsonEachCursor {
  sqlite3_vtab_cursor base;  /* Base class - must be first */
  u32 iRowid;                /* The rowid */
  u32 iEnd;                  /* EOF when iRowid equals or exceeds this value */
  u32 iNbytes;
  float *zJson;               /* Input array */
};

/* Constructor for the json_each virtual table.
** Usage:
** CREATE VIRTUAL TABLE temp.t1 USING float_each(
**   N=1, prefix=col, suffix=, rowname=nrow);
**
** Arguments:
**   N = number of columns.
**   prefix and suffix: e.g.,
**     Given (9,col), columns will be named nrow,col1 ... col9.
**     Given (10,hr,a), columns will be named nrow,hr01a ... hr10a.
** All arguments are optional but must appear in order,
** with or without argument names. Do not quote strings.
*/
static int jsonEachConnect(
  sqlite3 *db,
  void *pAux,
  int argc, const char *const*argv,
  sqlite3_vtab **ppVtab,
  char **pzErr
){
  CsvTable *pNew = NULL;        /* The CsvTable object to construct */
  int rc;

/* Column numbers
** The xBestIndex method assumes that the JSON is the first column.
** Should this ever changes, be sure to update the xBestIndex method.
*/
#define JEACH_JSON    0
#define JEACH_KEY     1
#define JEACH_VALUE   2

  UNUSED_PARAM(pzErr);
  UNUSED_PARAM(pAux);

  int nCol = 1; int nColDigits = 1;
  std::string zColPrefix("col");
  std::string zColSuffix("");
  std::string zRowName("nrow");
  /* Determine the parameter N = number of columns.
  ** Take the first user argument if it exists, e.g. argv[3].
  ** Otherwise default to N = 1.
  */
  if (argc > 3) {
    std::string target(argv[3]);
    // Pattern should match the number in 'N = 123' or simply '123'.
    // Whitespace optional betweek each token.
    std::regex rx(R"""((?:\s*N\s*=)?\s*(\d+)\s*)"""); // no double backslashes with raw string literal
    std::smatch match;
    if (regex_match(target.cbegin(), target.cend(), match, rx)) {
      // Regex found
      int nColFound = std::stoi(match.str(1));
      if (nColFound >= 1) {
        nCol = nColFound;
      } else {
        //silently ignore the error
      }
    } else {
      //silently ignore the error
    }
  }
  if (argc > 4) {
    std::string target(argv[4]);
    // prefix=col
    std::regex rx(R"""((?:\s*prefix\s*=)?\s*(\w+)\s*)""");
    std::smatch match;
    if (regex_match(target.cbegin(), target.cend(), match, rx)) {
      // Regex found
      zColPrefix = match.str(1);
    } else {
      //silently ignore the error
    }
  }
  if (argc > 5) {
    std::string target(argv[5]);
    // suffix=a
    std::regex rx(R"""((?:\s*suffix\s*=)?\s*(\w+)\s*)""");
    std::smatch match;
    if (regex_match(target.cbegin(), target.cend(), match, rx)) {
      // Regex found
      zColSuffix = match.str(1);
    } else {
      //silently ignore the error
    }
  }
  if (argc > 6) {
    std::string target(argv[6]);
    // rowname=a
    std::regex rx(R"""((?:\s*rowname\s*=)?\s*(\w+)\s*)""");
    std::smatch match;
    if (regex_match(target.cbegin(), target.cend(), match, rx)) {
      // Regex found
      zRowName = match.str(1);
    } else {
      //silently ignore the error
    }
  }
  nColDigits = 1+(int)log10(nCol); // e.g. 1 -> 1, 9 -> 1, 10 -> 2, 99 -> 2, etc.
  std::ostringstream schema;
  schema << "CREATE TABLE x(data HIDDEN";
  schema << ",\"" << zRowName << "\"";
  for (int i = 1; i <= nCol; i++) {
    schema << ",\"" << zColPrefix
      << std::setw(nColDigits) << std::setfill('0') << i
      << zColSuffix << "\"";
  }
  schema << ")";

  rc = sqlite3_declare_vtab(db,schema.str().data());
  if( rc==SQLITE_OK ){
    pNew = (CsvTable *)sqlite3_malloc( sizeof(*pNew) );
    *ppVtab = (sqlite3_vtab*)pNew;
    if( pNew==0 ) {
      return SQLITE_NOMEM;
    }
    memset(pNew, 0, sizeof(*pNew));
    pNew->nCol = nCol;
    sqlite3_vtab_config(db, SQLITE_VTAB_INNOCUOUS);
  }
  return rc;
}

/* destructor for json_each virtual table */
static int jsonEachDisconnect(sqlite3_vtab *pVtab){
  CsvTable *p = (CsvTable*)pVtab;
  sqlite3_free(p);
  return SQLITE_OK;
}

/*
** The xConnect and xCreate methods do the same thing, but they must be
** different so that the virtual table is not an eponymous virtual table.
*/
static int jsonEachCreate(
  sqlite3 *db,
  void *pAux,
  int argc, const char *const*argv,
  sqlite3_vtab **ppVtab,
  char **pzErr
){
 return jsonEachConnect(db, pAux, argc, argv, ppVtab, pzErr);
}


/* constructor for a JsonEachCursor object for json_each(). */
static int jsonEachOpenEach(sqlite3_vtab *p, sqlite3_vtab_cursor **ppCursor){
  JsonEachCursor *pCur;

  UNUSED_PARAM(p);
  pCur = (JsonEachCursor *)sqlite3_malloc( sizeof(*pCur) );
  if( pCur==0 ) return SQLITE_NOMEM;
  memset(pCur, 0, sizeof(*pCur));
  *ppCursor = &pCur->base;
  return SQLITE_OK;
}

/* constructor for a JsonEachCursor object for json_tree(). */
static int jsonEachOpenTree(sqlite3_vtab *p, sqlite3_vtab_cursor **ppCursor){
  int rc = jsonEachOpenEach(p, ppCursor);
  if( rc==SQLITE_OK ){
    JsonEachCursor *pCur = (JsonEachCursor*)*ppCursor;
  }
  return rc;
}

/* Reset a JsonEachCursor back to its original state.  Free any memory
** held. */
static void jsonEachCursorReset(JsonEachCursor *p){
  sqlite3_free(p->zJson);
  p->iRowid = 0;
  p->iEnd = 0;
  p->iNbytes = 0;
  p->zJson = 0;
}

/* Destructor for a jsonEachCursor object */
static int jsonEachClose(sqlite3_vtab_cursor *cur){
  JsonEachCursor *p = (JsonEachCursor*)cur;
  jsonEachCursorReset(p);
  sqlite3_free(cur);
  return SQLITE_OK;
}

/* Return TRUE if the jsonEachCursor object has been advanced off the end
** of the JSON object */
static int jsonEachEof(sqlite3_vtab_cursor *cur){
  JsonEachCursor *p = (JsonEachCursor*)cur;
  return p->iRowid >= p->iEnd;
}

/* Advance the cursor to the next element for json_tree() */
static int jsonEachNext(sqlite3_vtab_cursor *cur){
  JsonEachCursor *p = (JsonEachCursor*)cur;
  p->iRowid++;
  return SQLITE_OK;
}

/* Return the value of a column */
static int jsonEachColumn(
  sqlite3_vtab_cursor *cur,   /* The cursor */
  sqlite3_context *ctx,       /* First argument to sqlite3_result_...() */
  int i                       /* Which column to return */
){
  JsonEachCursor *p = (JsonEachCursor*)cur;
  CsvTable *pTab = (CsvTable*)cur->pVtab;
  assert(p->iRowid < p->iEnd);
  switch( i ){
    case JEACH_JSON: {
      sqlite3_result_blob(ctx, p->zJson, p->iNbytes, SQLITE_TRANSIENT);
      break;
    }
    case JEACH_KEY: {
      sqlite3_result_int64(ctx, (sqlite3_int64)(1 + p->iRowid));
      break;
    }
    default: {
      int iCol = i - JEACH_VALUE;
      assert( i >= JEACH_VALUE );
      assert( i < pTab->nCol );
      sqlite3_result_double(ctx, (double)p->zJson[pTab->nCol * p->iRowid + iCol]);
      break;
    }
  }
  return SQLITE_OK;
}

/* Return the current rowid value */
static int jsonEachRowid(sqlite3_vtab_cursor *cur, sqlite_int64 *pRowid){
  JsonEachCursor *p = (JsonEachCursor*)cur;
  *pRowid = (sqlite_int64)p->iRowid;
  return SQLITE_OK;
}

/* Required to avoid C++ compiler error, e.g.
**   'error: cannot increment a pointer to incomplete type'
** From sqlite3.h:6905
*/
struct sqlite3_index_constraint {
  int iColumn;              /* Column constrained.  -1 for ROWID */
  unsigned char op;         /* Constraint operator */
  unsigned char usable;     /* True if this constraint is usable */
  int iTermOffset;          /* Used internally - xBestIndex should ignore */
};

/* The query strategy is to look for an equality constraint on the json
** column.  Without such a constraint, the table cannot operate.  idxNum is
** 1 if the constraint is found, 3 if the constraint and zRoot are found,
** and 0 otherwise.
*/
static int jsonEachBestIndex(
  sqlite3_vtab *tab,
  sqlite3_index_info *pIdxInfo
){
  UNUSED_PARAM(tab);
  int i;                     /* Loop counter or computed array index */
  int aIdx[2];               /* Index of constraints for JSON and NROW */
  int unusableMask = 0;      /* Mask of unusable JSON and NROW constraints */
  int idxMask = 0;           /* Mask of usable == constraints JSON and NROW */
  const struct sqlite3_index_constraint *pConstraint;

  /* This implementation assumes that JSON is the first
  ** columns in the table */
  aIdx[0] = aIdx[1] = -1;
  pConstraint = (const struct sqlite3_index_constraint *)pIdxInfo->aConstraint;
  for(i=0; i<pIdxInfo->nConstraint; i++, pConstraint++){
    int iCol;
    int iMask;
    if( pConstraint->iColumn < 0 ) continue; // -1 for ROWID
    if( pConstraint->iColumn > JEACH_JSON ) continue;
    iCol = pConstraint->iColumn;
    assert( iCol==0 || iCol==1 );
    testcase( iCol==0 );
    iMask = 1 << iCol;
    if( pConstraint->usable==0 ){
      unusableMask |= iMask;
    }else if( pConstraint->op==SQLITE_INDEX_CONSTRAINT_EQ ){
      aIdx[iCol] = i;
      idxMask |= iMask;
    }
  }
  if( (unusableMask & ~idxMask)!=0 ){
    /* If there are any unusable constraints on JSON or NROW, then reject
    ** this entire plan */
    return SQLITE_CONSTRAINT;
  }
  if( aIdx[0]<0 ){
    /* No JSON input.  Leave estimatedCost at the huge value that it was
    ** initialized to to discourage the query planner from selecting this
    ** plan. */
    pIdxInfo->idxNum = 0;
  }else{
    pIdxInfo->estimatedCost = 1.0;
    i = aIdx[0];
    pIdxInfo->aConstraintUsage[i].argvIndex = 1;
    pIdxInfo->aConstraintUsage[i].omit = 1;
    //if( aIdx[1]<0 ){
      pIdxInfo->idxNum = 1;  /* Only JSON supplied.  Plan 1 */
    //}
  }
  return SQLITE_OK;
}

/* Start a search on a new JSON string */
static int jsonEachFilter(
  sqlite3_vtab_cursor *cur,
  int idxNum, const char *idxStr,
  int argc, sqlite3_value **argv
){
  JsonEachCursor *pCur = (JsonEachCursor*)cur;
  CsvTable *pTab = (CsvTable*)cur->pVtab;
  int bytesPerRow = pTab->nCol * sizeof(float);
  const float *z;
  const char *zRoot = 0;
  sqlite3_int64 n;
  UNUSED_PARAM(idxStr);
  assert( argc == 1 );
  jsonEachCursorReset(pCur);
  if( idxNum==0 ) return SQLITE_OK;

  n = sqlite3_value_bytes(argv[0]);
  pCur->iNbytes = n;
  pCur->iEnd = (u32) (n / bytesPerRow);
  z = (const float*)sqlite3_value_blob(argv[0]);
  if( z==0 ) return SQLITE_OK; // If NUL in, then NUL out.
  if( (n % bytesPerRow) != 0 ){
    int rc = SQLITE_NOMEM;
    sqlite3_free(cur->pVtab->zErrMsg);
    cur->pVtab->zErrMsg = sqlite3_mprintf("malformed array data");
    if( cur->pVtab->zErrMsg ) rc = SQLITE_ERROR;
    jsonEachCursorReset(pCur);
    return rc;
  }else{
    pCur->zJson = (float *)sqlite3_malloc64( n );
    if( pCur->zJson==0 ) return SQLITE_NOMEM;
    memcpy(pCur->zJson, z, (size_t)n);
    pCur->iRowid = 0;
  }
  return SQLITE_OK;
}

/* The methods of the json_each virtual table */
static sqlite3_module jsonEachModule = {
  0,                         /* iVersion */
  jsonEachCreate,                         /* xCreate */
  jsonEachConnect,           /* xConnect */
  jsonEachBestIndex,         /* xBestIndex */
  jsonEachDisconnect,        /* xDisconnect */
  jsonEachDisconnect,        /* xDestroy */
  jsonEachOpenEach,          /* xOpen - open a cursor */
  jsonEachClose,             /* xClose - close a cursor */
  jsonEachFilter,            /* xFilter - configure scan constraints */
  jsonEachNext,              /* xNext - advance a cursor */
  jsonEachEof,               /* xEof - check for end of scan */
  jsonEachColumn,            /* xColumn - read data */
  jsonEachRowid,             /* xRowid - read data */
  0,                         /* xUpdate */
  0,                         /* xBegin */
  0,                         /* xSync */
  0,                         /* xCommit */
  0,                         /* xRollback */
  0,                         /* xFindMethod */
  0,                         /* xRename */
  0,                         /* xSavepoint */
  0,                         /* xRelease */
  0,                         /* xRollbackTo */
  0                          /* xShadowName */
};

#endif /* SQLITE_OMIT_VIRTUALTABLE */

/****************************************************************************
** The following routines are the only publically visible identifiers in this
** file.  Call the following routines in order to register the various SQL
** functions and the virtual table implemented by this file.
****************************************************************************/

int sqlite3Json1Init(sqlite3 *db){
  int rc = SQLITE_OK;
  unsigned int i;
  static const struct {
     const char *zName;
     void (*xFunc)(sqlite3_context*,int,sqlite3_value**);
     int nArg;
     int flag;
  } aFunc[] = {
    
    { "float32Null",       float32NullFunc,     1, 0                          },

#if SQLITE_DEBUG
    /* DEBUG and TESTING functions */
    { "json_parse",        jsonParseFunc,       1, 0                          },
    { "json_test1",        jsonTest1Func,       1, 0                          },
#endif
  };
  static const struct {
     const char *zName;
     int nArg;
     void (*xStep)(sqlite3_context*,int,sqlite3_value**);
     void (*xFinal)(sqlite3_context*);
     void (*xValue)(sqlite3_context*);
  } aAgg[] = {
    // pass
  };
#ifndef SQLITE_OMIT_VIRTUALTABLE
  static const struct {
     const char *zName;
     sqlite3_module *pModule;
  } aMod[] = {
    { "float_each",            &jsonEachModule              },
  };
#endif
  static const int enc = 
       SQLITE_UTF8 |
       SQLITE_DETERMINISTIC |
       SQLITE_INNOCUOUS;
  for(i=0; i<sizeof(aFunc)/sizeof(aFunc[0]) && rc==SQLITE_OK; i++){
    rc = sqlite3_create_function(db, aFunc[i].zName, aFunc[i].nArg, enc,
                                 (void*)&aFunc[i].flag,
                                 aFunc[i].xFunc, 0, 0);
  }

#ifndef SQLITE_OMIT_VIRTUALTABLE
  for(i=0; i<sizeof(aMod)/sizeof(aMod[0]) && rc==SQLITE_OK; i++){
    rc = sqlite3_create_module(db, aMod[i].zName, aMod[i].pModule, 0);
  }
#endif
  return rc;
}


#ifndef SQLITE_CORE
#ifdef __cplusplus
extern "C"
#endif
#ifdef _WIN32
__declspec(dllexport)
#endif
int sqlite3_floataway_init(
  sqlite3 *db, 
  char **pzErrMsg, 
  const sqlite3_api_routines *pApi
){
  SQLITE_EXTENSION_INIT2(pApi);
  (void)pzErrMsg;  /* Unused parameter */
  return sqlite3Json1Init(db);
}
#endif
#endif /* !defined(SQLITE_CORE) || defined(SQLITE_ENABLE_JSON1) */
