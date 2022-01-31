.load floataway
DROP TABLE IF EXISTS temp.t1;
--CREATE VIRTUAL TABLE IF NOT EXISTS temp.t1 USING float_each( 5 ,'filename=thefile.csv', filename = SELECT 'thefile' || chr(10) || '.csv' );
CREATE VIRTUAL TABLE IF NOT EXISTS temp.t1 USING float_each( 5 );
select a.rowid, a.key, a.value, a.json
FROM temp.t1(
x'BA498A41'
|| x'C5208841'
|| x'8B6C8641'
|| x'6ABC8541'
|| x'60E58541'
|| x'91ED8E41'
) a
WHERE key > 2 AND key < 5;

/* Result
argc=6
argv[0]=float_each
argv[1]=temp
argv[2]=t1
argv[3]=5
argv[4]='filename=thefile.csv'
argv[5]=filename = SELECT 'thefile' || chr(10) || '.csv'
*/
