REM if dynamic linking, also need to distribute the libgcc and libstdc++ DLLs.
g++ -g -shared -std=c++11 floataway.c -o floataway_mingw_x64.dll

REM If static linking, no other DLLs required.
g++ -g -shared -std=c++11 -static floataway.c -o floataway_mingw_x64_static.dll

g++ -g -fPIC -shared -std=c++11 floataway.c -o floataway_mingw_x64.so
