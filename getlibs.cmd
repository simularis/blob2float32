REM set mingwbin=%HOMEDRIVE%%HOMEPATH%\AppData\Local\Continuum\anaconda3\Library\mingw-w64\bin
set mingwbin=%HOMEDRIVE%%HOMEPATH%\AppData\Local\Continuum\anaconda3\envs\myclone\Library\mingw-w64\bin

copy "%mingwbin%\libgcc_s_seh-1.dll .\libgcc_s_seh-1.dll"
copy "%mingwbin%\libstdc++-6.dll" ".\libstdc++-6.dll"
