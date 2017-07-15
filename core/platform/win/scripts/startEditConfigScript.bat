@echo off
setlocal
cd /d %~dp0

cd ../../..
notepad ./config/iwp-win.ini &
exit 1