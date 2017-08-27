@echo off
setlocal
cd /d "%~dp0"

REM --------- START IWPCLI -------------
REM You need to set the ShowQEMUWindow 
REM setting to yes so that the QEMU
REM window shows when using this batch
REM file.
REM ------------------------------------
echo Starting InstantWP
start /b run-iwpcli.bat start &

REM --------- START GUI -------------
echo Start InstantWP Control Panel
cd ..
start /b controlpanel\InstantWP.exe &

exit /b
