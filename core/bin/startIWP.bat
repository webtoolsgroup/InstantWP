@echo off 
setlocal

%~d0
cd /d %~dp0

echo IWP Startup
title IWP Startup

iwpcli start 
cd ..
start controlpanel\InstantWP.exe 
exit /b