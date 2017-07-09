@echo off
setlocal
cd /d %~dp0

echo Starting InstantWP
cd bin
startIWP.bat &
exit /b
