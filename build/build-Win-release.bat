@echo off
setlocal
cd /d %~dp0

echo ----------------------------
echo Win IWP Release Build Script
echo ----------------------------

REM get the version numbers
set /p IWP_VERSION=< C:\Users\paperspace\Documents\GitHub\InstantWP\build\IWP_VERSION.txt 
set /p VM_VERSION=< C:\Users\paperspace\Documents\GitHub\InstantWP\build\VM_VERSION.txt 

REM some constants
mkdir C:\Users\paperspace\Documents\GitHub\InstantWP\build\release
SET REL_ROOT=C:\Users\paperspace\Documents\GitHub\InstantWP\build\release
SET SOURCE_DIR=C:\Users\paperspace\Documents\GitHub\InstantWP
SET VM_FILE=%VM_VERSION%.qcow2

REM set release root
SET REL_DIR=%REL_ROOT%\IWP-"%IWP_VERSION%"-Win

echo Making release directory %REL_DIR%
mkdir %REL_DIR%

echo Making build directories...
mkdir %REL_DIR%\bin
mkdir %REL_DIR%\bin\lib
mkdir %REL_DIR%\images
mkdir %REL_DIR%\config
mkdir %REL_DIR%\controlpanel
mkdir %REL_DIR%\docs
mkdir %REL_DIR%\docs\images
mkdir %REL_DIR%\platform
mkdir %REL_DIR%\platform\win
mkdir %REL_DIR%\vm

echo Copying files...

REM startup files
copy %SOURCE_DIR%\core\iwpcli.exe %REL_DIR%\
copy %SOURCE_DIR%\core\Start-InstantWP.bat %REL_DIR%\
copy %SOURCE_DIR%\core\ReadMe\ReadMe-First-Windows.html %REL_DIR%\

REM bin directory
copy %SOURCE_DIR%\core\bin\run-iwpcli.bat %REL_DIR%\bin\
copy %SOURCE_DIR%\core\bin\iwp.exe %REL_DIR%\bin\
copy %SOURCE_DIR%\core\bin\HideQEMUWindow.exe %REL_DIR%\bin\
copy %SOURCE_DIR%\core\bin\HideQEMUWindow.bat %REL_DIR%\bin\
REM copy %SOURCE_DIR%\core\bin\Start-InstantWP-Win.exe %REL_DIR%\bin\
REM copy %SOURCE_DIR%\core\bin\start-iwp-win.exe %REL_DIR%\bin\
REM copy %SOURCE_DIR%\core\bin\Start-IWP-Show-QEMU.bat %REL_DIR%\bin\
xcopy /s /e %SOURCE_DIR%\core\bin\lib %REL_DIR%\bin\lib

REM config directory
copy %SOURCE_DIR%\core\config\iwp-win.ini %REL_DIR%\config\

REM doc directory
copy %SOURCE_DIR%\core\docs\about.html %REL_DIR%\docs
copy %SOURCE_DIR%\core\docs\documentation.html %REL_DIR%\docs\
copy %SOURCE_DIR%\core\docs\InstantWP-User-Guide.pdf %REL_DIR%\docs\
copy %SOURCE_DIR%\core\docs\LICENSE.txt %REL_DIR%\docs\LICENSE.txt
xcopy /s %SOURCE_DIR%\core\docs\images  %REL_DIR%\docs\images

REM control panel dir
xcopy /s /e %SOURCE_DIR%\core\controlpanel\distribute %REL_DIR%\controlpanel

REM htdocs
REM xcopy /s %SOURCE_DIR%\core\htdocs %REL_DIR%\htdocs

REM images
xcopy /s %SOURCE_DIR%\core\images %REL_DIR%\images

REM platform directory
xcopy /s %SOURCE_DIR%\core\platform\win %REL_DIR%\platform\win

REM vm directory
copy %SOURCE_DIR%\core\vm\%VM_FILE% %REL_DIR%\vm

REM update the version numbers
perl -i.bak -ape "s/IWP_VERSION/%IWP_VERSION%/g" %REL_DIR%\docs\about.html & del  %REL_DIR%\docs\about.html.bak
perl -i.bak -ape "s/IWP_VERSION/%IWP_VERSION%/g" %REL_DIR%\config\iwp-win.ini & del  %REL_DIR%\config\iwp-win.ini.bak
perl -i.bak -ape "s/VM_VERSION/%VM_VERSION%/g" %REL_DIR%\config\iwp-win.ini & del %REL_DIR%\config\iwp-win.ini.bak

echo Done!