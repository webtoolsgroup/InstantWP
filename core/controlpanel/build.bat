@echo off
setlocal
cd /d %~dp0

echo -------------------------------
echo Win InstantWP.exe Build Script
echo -------------------------------

echo Removing old version of app...
del InstantWP.exe
del distribute\InstantWP.exe

echo Building new exe...
"C:\Program Files (x86)\Racket\raco.exe" exe   --ico ./resource/IWP.ico --gui InstantWP.rkt

echo Creating distribution...
"C:\Program Files (x86)\Racket\raco.exe" distribute distribute InstantWP.exe
echo Done creating InstantWP.exe!