@echo off
setlocal
cd /d "%~dp0"

REM --------- START IWPCLI -------------
echo Starting InstantWP
cd bin
start /b start-iwp-win.exe &

REM --------- START GUI -------------
echo Start InstantWP Control Panel
cd ..
start /b controlpanel\InstantWP.exe &
exit /b
