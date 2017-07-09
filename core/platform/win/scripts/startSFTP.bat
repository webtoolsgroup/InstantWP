@echo off
setlocal
cd /d %~dp0

cd ../../..

cd platform/win/sync

WinSCP.exe &

exit 1