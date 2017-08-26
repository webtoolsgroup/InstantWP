@echo off
setlocal
cd /d "%~dp0"

REM --- wait 30 seconds before hiding QEMU window ---
timeout /t 30 /nobreak > NUL

REM --------- HIDE QEMU -------------
start /b  HideQEMUWindow.exe &

exit /b
