@echo off 
setlocal
%~d0
cd /d %~dp0

echo IWP Startup
title IWP Startup

echo Start IWPServer
start start-iwp-win.exe &

echo Start InstantWP Control Panel
cd ..
start controlpanel\InstantWP.exe &
exit /b