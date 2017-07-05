@echo off 
setlocal

%~d0
cd /d %~dp0

echo IWP Startup
title IWP Startup

set IWPROOT=%cd%
start controlpanel\InstantWP.exe 
iwpcli start 
iwpcli hide_vm
%IWPROOT%\platform\win\nircmd\nircmdc.exe win hide title "IWP Startup"
exit /b