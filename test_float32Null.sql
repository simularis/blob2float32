select i, hex(float32Null(i)) as result
from (select 0 as "i"
UNION select 1
UNION select 2
UNION select 3
UNION select 4
UNION select 5
UNION select 6
UNION select 7)
