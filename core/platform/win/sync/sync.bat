@echo off
setlocal
cd /d %~dp0

REM show args...
REM echo %1 %2 %3 %4 %5 %6 %7

REM *************** IWP File Sync *******************
REM *************************************************
REM Stores the PID of this process and runs the sync
REM *************************************************

REM Delete any old pid files
del *.pid

REM Get the PID of the cmd process
set T=%TEMP%\iwpsync.pid.tmp
wmic process where (Name="WMIC.exe" AND CommandLine LIKE "%%%TIME%%%") get ParentProcessId /value | find "ParentProcessId" >%T%
set /P A=<%T%

REM The value "%A%" is like "ParentProcessId=2008"
REM echo %A:~16%

REM Store the PID in a file
echo %A:~16% > sync.pid

REM Run the winscp sync in the loop

:loop
REM winscp.com /script=scp.script /parameter "root" "root" "127.0.0.1" "10022" "C:\Users\paperspace\Downloads\WinSCP-5.9.2-Portable\wordpress" "/usr/local/www/apache24/data/wordpress/"
winscp.com /script=scp.script /parameter %1 %2 %3 %4 %5 %6
if %errorlevel% neq 0 exit /b %errorlevel%
timeout /t %7
goto loop