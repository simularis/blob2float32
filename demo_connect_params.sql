.load floataway
.mode table

DROP TABLE IF EXISTS temp.t1;
CREATE VIRTUAL TABLE temp.t1 USING float_each();
select hex(a.data), a.nrow, a.col1
FROM temp.t1(
x'BA498A41'
|| x'C5208841'
|| x'8B6C8641'
|| x'6ABC8541'
|| x'60E58541'
|| x'91ED8E41'
) a
;

DROP TABLE IF EXISTS temp.t2;
CREATE VIRTUAL TABLE temp.t2 USING float_each(N=2,prefix=hr,suffix=a);
select hex(a.data), a.nrow, a.hr1a, a.hr2a
FROM temp.t2(
x'BA498A41'
|| x'C5208841'
|| x'8B6C8641'
|| x'6ABC8541'
|| x'60E58541'
|| x'91ED8E41'
) a
;

select hex(a.data), a.nrow, a.hr1a, a.hr2a
FROM temp.t2(
x'BA498A41'
|| x'C5208841'
|| x'8B6C8641'
|| x'6ABC8541'
|| x'60E58541'
|| x'91ED8E41'
) a
WHERE nrow = 1
;

DROP TABLE IF EXISTS temp.t3;
CREATE VIRTUAL TABLE temp.t3 USING float_each(N=3,prefix=hr,suffix=a,rowname=daynum);
select a.* FROM temp.t3(x'BA498A41C52088418B6C86416ABC854160E5854191ED8E41') a
;
