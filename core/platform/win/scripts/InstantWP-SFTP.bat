@echo off
setlocal
cd /d %~dp0

cd ../../..

cd platform/win/sftp

WinSCP.exe &

exit 1