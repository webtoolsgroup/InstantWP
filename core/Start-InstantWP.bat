@echo off 
setlocal
cd /d %~dp0

echo IWP Startup
title IWP Startup
set IWPROOT=%~dp0
iwpcli start 
iwpcli hide_vm
cd bin
start InstantWP.exe %IWPROOT%
%IWPROOT%\platform\win\nircmd\nircmdc.exe win hide title "IWP Startup"
exit /b