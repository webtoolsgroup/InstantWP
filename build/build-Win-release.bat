@echo off
setlocal
cd /d %~dp0

echo ----------------------------
echo Win IWP Release Build Script
echo ----------------------------

REM some constants
mkdir C:\Users\paperspace\Documents\GitHub\InstantWP\build\release
SET REL_ROOT=C:\Users\paperspace\Documents\GitHub\InstantWP\build\release
SET SOURCE_DIR=C:\Users\paperspace\Documents\GitHub\InstantWP
SET VM_FILE=iwpserver-2.0.0.qcow2

REM set release root
SET REL_DIR=%REL_ROOT%\%1

echo Making release directory %REL_DIR%
mkdir %REL_DIR%

echo Making build directories...
mkdir %REL_DIR%\bin
mkdir %REL_DIR%\bin\images
mkdir %REL_DIR%\config
mkdir %REL_DIR%\docs
mkdir %REL_DIR%\docs\images
mkdir %REL_DIR%\platform
mkdir %REL_DIR%\platform\win
mkdir %REL_DIR%\htdocs
mkdir %REL_DIR%\htdocs\wordpress
mkdir %REL_DIR%\vm

echo Copying files...

REM startup files
copy %SOURCE_DIR%\core\iwpcli.exe %REL_DIR%\
copy %SOURCE_DIR%\core\Start-InstantWP.bat %REL_DIR%\
copy %SOURCE_DIR%\core\Quit-InstantWP.bat %REL_DIR%\
copy %SOURCE_DIR%\core\ReadMe\ReadMe-WIN.txt %REL_DIR%\

REM bin directory
copy %SOURCE_DIR%\core\bin\InstantWP.exe %REL_DIR%\bin\
xcopy /s %SOURCE_DIR%\core\bin\images %REL_DIR%\bin\images
copy %SOURCE_DIR%\core\bin\iwp.exe %REL_DIR%\bin\

REM config directory
copy %SOURCE_DIR%\core\config\iwp-win.ini %REL_DIR%\config\

REM doc directory
copy %SOURCE_DIR%\core\docs\about.html %REL_DIR%\docs
copy %SOURCE_DIR%\core\docs\documentation.html %REL_DIR%\docs\
copy %SOURCE_DIR%\core\docs\LICENSE.txt %REL_DIR%\docs\LICENSE.txt
xcopy /s %SOURCE_DIR%\core\docs\images  %REL_DIR%\docs\images


REM htdocs
xcopy /s %SOURCE_DIR%\core\htdocs %REL_DIR%\htdocs


REM platform directory

xcopy /s %SOURCE_DIR%\core\platform\win %REL_DIR%\platform\win

REM vm directory
copy %SOURCE_DIR%\core\vm\%VM_FILE% %REL_DIR%\vm

REM zipping the release
REM echo Making release zip $REL_ROOT/$1.zip
REM cd %REL_DIR%
REM zip -r $REL_ROOT/$1.zip ./$1 -x "*.DS_Store"

echo Done!