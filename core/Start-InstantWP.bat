@echo off
setlocal
cd /d "%~dp0"

REM --------- START IWPCLI -------------
echo Starting InstantWP
cd bin
start start-iwp-win.exe &

REM --------- START GUI -------------
echo Start InstantWP Control Panel
cd ..
start controlpanel\InstantWP.exe &
exit /b
