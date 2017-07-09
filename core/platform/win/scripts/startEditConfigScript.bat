@echo off
setlocal
cd /d %~dp0

cd ../../..
start ./config/iwp-osx.ini &
exit 1