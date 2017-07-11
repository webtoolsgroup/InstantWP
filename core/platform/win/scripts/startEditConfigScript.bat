@echo off
setlocal
cd /d %~dp0

cd ../../..
start ./config/iwp-win.ini &
exit 1