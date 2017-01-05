@echo off 
setlocal
cd /d %~dp0

echo IWP Quitting...
iwpcli show_vm
iwpcli quit
echo Please wait...
timeout 30
exit