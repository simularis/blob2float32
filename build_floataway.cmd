REM if dynamic linking, also need to distribute the libgcc and libstdc++ DLLs.
g++ -g -shared -std=c++11 floataway.c -o floataway.dll

REM If static linking, no other DLLs required.
REM g++ -g -shared -std=c++11 -static-libstdc++ -static-libgcc floataway.c -o floataway.dll
