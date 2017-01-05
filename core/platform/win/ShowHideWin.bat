@echo off
setlocal
cd /d %~dp0

REM show args...
REM echo %1 %2 %3 %4 %5 %6 %7

REM Wrapper for nircmdc.exe

%~dp0/nircmd/nircmdc.exe win togglehide ititle "%1"