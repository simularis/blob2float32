/*
select a.rowid, a.key, a.value, hex(a.json), ifnull(a.value,'null') as is_null
FROM float_each(
x'BA498A41'
|| x'C5208841'
|| x'8B6C8641'
|| x'6ABC8541'
|| x'60E58541'
|| x'91ED8E41'
|| x'ffffffff'
|| x'ffffefff'
|| x'7fffffff'
|| x'7fff8fff'
|| x'7fff80ff'
|| x'7f8fffff'
|| x'7f8eeeff'
|| x'7f80ffff'
|| x'0000C0FF'
|| x'0000C07F'
) a
--WHERE a.value IS NULL
;

select b.rowid, b.*, b.json
FROM float_each(
x''
) b
;
select c.rowid, c.*, c.json
FROM float_each(
'hello world!'
) c
;
select e.rowid, e.*, e.json
FROM float_each e
where e.json = x'BA498A41'
;
select f.rowid, f.*, f.json
FROM float_each(NULL
) f
;
select d.rowid, d.*, d.json
FROM float_each(
3,4
) d
;
*/
select a.rowid, a.key, a.value, a.json
FROM float_each(
x'BA498A41'
|| x'C5208841'
|| x'8B6C8641'
|| x'6ABC8541'
|| x'60E58541'
|| x'91ED8E41'
) a
WHERE key > 2 AND key < 5
;
