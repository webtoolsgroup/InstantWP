@echo off
setlocal
cd /d %~dp0

echo ----------------------------
echo Win IWP Release Build Script
echo ----------------------------

REM some constants
SET REL_ROOT=C:\Users\paperspace\Documents\GitHub\InstantWordPress\zzRelease
SET SOURCE_DIR=C:\Users\paperspace\Documents\GitHub\InstantWordPress
SET VM_FILE=iwpserver-1.1.qcow2

REM set release root
SET REL_DIR=%REL_ROOT%\%1

echo Making release directory %REL_DIR%
mkdir %REL_DIR%

echo Making build directories...
mkdir %REL_DIR%\bin
mkdir %REL_DIR%\bin\images
mkdir %REL_DIR%\config
mkdir %REL_DIR%\docs
mkdir %REL_DIR%\platform
mkdir %REL_DIR%\platform\win
mkdir %REL_DIR%\htdocs
mkdir %REL_DIR%\vm

echo Copying files...

REM startup files
copy %SOURCE_DIR%\MainDev\iwpcli.exe %REL_DIR%\
copy %SOURCE_DIR%\MainDev\Start-InstantWP.bat %REL_DIR%\
copy %SOURCE_DIR%\MainDev\Quit-InstantWP.bat %REL_DIR%\
copy %SOURCE_DIR%\MainDev\ReadMe\ReadMe-WIN.txt %REL_DIR%\

REM bin directory
copy %SOURCE_DIR%\MainDev\bin\InstantWP.exe %REL_DIR%\bin\
xcopy /s %SOURCE_DIR%\MainDev\bin\images %REL_DIR%\bin\images
copy %SOURCE_DIR%\MainDev\bin\iwp.exe %REL_DIR%\bin\

REM config directory
copy %SOURCE_DIR%\MainDev\config\iwp-win.ini %REL_DIR%\config\

REM doc directory
copy %SOURCE_DIR%\MainDev\docs\about.html %REL_DIR%\docs
copy %SOURCE_DIR%\MainDev\docs\documentation.html %REL_DIR%\docs\

REM htdocs
xcopy /s %SOURCE_DIR%\MainDev\htdocs %REL_DIR%\htdocs


REM platform directory

xcopy /s %SOURCE_DIR%\MainDev\platform\win %REL_DIR%\platform\win

REM vm directory
copy %SOURCE_DIR%\MainDev\vm\%VM_FILE% %REL_DIR%\vm

REM zipping the release
REM echo Making release zip $REL_ROOT/$1.zip
REM cd %REL_DIR%
REM zip -r $REL_ROOT/$1.zip ./$1 -x "*.DS_Store"

echo Done!