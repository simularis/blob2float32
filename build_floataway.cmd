REM if dynamic linking, also need to distribute the libgcc and libstdc++ DLLs.
REM g++ -g -shared -std=c++11 floataway.c -o floataway.dll

REM If static linking, no other DLLs required.
g++ -g -shared -std=c++11 -static-libstdc++ -static-libgcc floataway.c -o floataway.dll
