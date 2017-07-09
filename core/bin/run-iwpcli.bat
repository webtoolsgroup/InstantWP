@echo off
setlocal
cd /d %~dp0
cd ../
iwpcli %1 &
exit 1
