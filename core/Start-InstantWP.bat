@echo off
setlocal
cd /d "%~dp0"

REM --------- START IWPCLI -------------
echo Starting InstantWP
start /min iwpcli start ^& exit

REM --------- START GUI -------------
echo Start InstantWP Control Panel
start /b controlpanel\InstantWP.exe &

REM --------- HIDE QEMU -------------
start /min  bin\HideQEMUWindow.exe &

exit /b
