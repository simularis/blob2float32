.load floataway
.mode table
.echo ON

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

CREATE VIRTUAL TABLE temp.t3 USING float_each(N=3,prefix=h,suffix=_kw,rowname=i);
SELECT b.* FROM temp.t0 a, temp.t3(a.mybytes) b;

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
