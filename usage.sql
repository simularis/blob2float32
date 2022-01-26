select load_extension('./blob2float32.dll');
select tofloat32(x'00000000'), tofloat32(x'ba498a41');