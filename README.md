SQLite extension(s) to read 32-bit floating point from BLOB values

## floataway
Provides the json_each module.

### Installing

Download the static binary DLL file from the releases and rename it to floataway.dll.
Save it to the folder next to your database.

### Loading the extension

* Instructions for sqlite3 CLI:
  * ```.load floataway```
* Instructions for sqlite3 CLI (alternate):
  * ```SELECT load_extension('./floataway.dll');```
* Instructions for GUI programs:
  * Use the menu > Tools > Load Extension, then select the DLL file.

### What and why

The 32-bit float type is not natively available in SQLite, which uses
double precision 64-bit floating point numbers for REAL typed values.

In order to reduce the storage required for lots of low precision data,
a long array of 32-bit floating point numbers may be stored in binary
representation in a single BLOB type column. This module reads them out as 
a 2d array.

### json_each usage

```CREATE VIRTUAL TABLE temp.t1 USING float_each(N=1, prefix=col, suffix=, rowname=nrow, fix=);```

Arguments:
1. N = number of columns.
2. prefix and
3. suffix: e.g.,
   * Given (9,col), columns will be named nrow,col1, ..., col9.
   * Given (10,h,_kw), columns will be named nrow,hr01_kw, ..., hr10_kw.
4. rowname = name to give the leftmost column. This column's value
    will be integers starting at 1.
5. fix = number of digits to round. Leave blank to skip rounding.

All arguments are optional but must appear in order,
with or without argument names. Do not quote strings in the arguments to float_each.

```sql
CREATE TABLE temp.t0 (
  mybytes BLOB
);
-- 000000000000803F0000004000004040000080400000C07F
INSERT INTO temp.t0 VALUES (
   x'00000000' || x'0000803F' || x'00000040'
|| x'0000C07F' || x'00004040' || x'00008040'
);

CREATE VIRTUAL TABLE temp.t1 USING float_each();
SELECT b.nrow, b.col1 FROM temp.t0 a, temp.t1(a.mybytes) b;
+------+------+
| nrow | col1 |
+------+------+
| 1    | 0.0  |
| 2    | 1.0  |
| 3    | 2.0  |
| 4    |      |
| 5    | 3.0  |
| 6    | 4.0  |
+------+------+

CREATE VIRTUAL TABLE temp.t3 USING float_each(N=3,prefix=h,suffix=_kw,rowname=i);
SELECT b.* FROM temp.t0 a, temp.t3(a.mybytes) b;
+---+-------+-------+-------+
| i | h1_kw | h2_kw | h3_kw |
+---+-------+-------+-------+
| 1 | 0.0   | 1.0   | 2.0   |
| 2 |       | 3.0   | 4.0   |
+---+-------+-------+-------+

CREATE VIRTUAL TABLE temp.r3 USING float_each(N=1,prefix=col,suffix=,rowname=,fix=3);
CREATE VIRTUAL TABLE temp.r6 USING float_each(N=1,prefix=col,suffix=,rowname=,fix=6);

WITH a AS (SELECT x'DB0F4940' as "mybytes")
SELECT 3 as "fix", 32 as "bits", b.* FROM a, temp.r3(a.mybytes) b
UNION
SELECT 6 as "fix", 32 as "bits", b.* FROM a, temp.r6(a.mybytes) b
UNION
SELECT 99 as "fix", 32 as "bits", b.* FROM a, temp.t1(a.mybytes) b
UNION
SELECT 99 as "fix", 64 as "bits", 2 as nrow, 3.141592653589793 as col1
ORDER BY bits, fix;
+-----+------+------+------------------+
| fix | bits | nrow |       col1       |
+-----+------+------+------------------+
| 3   | 32   | 1    | 3.142            |
| 6   | 32   | 1    | 3.141593         |
| 99  | 32   | 1    | 3.14159274101257 |
| 99  | 64   | 2    | 3.14159265358979 |
+-----+------+------+------------------+
```

### Details

1. Because the number of columns is a user input, float_each cannot work as
an eponymous virtual table. It determines the schema only when a user
creates a virtual table using the module and specifies N.

2. It treats the binary data as little-endian.
