@echo off 
setlocal
echo "Clean up any processes left behind..."
REM Shut down any ShowHideWin processes 
for /l %%A in (1,1,5) do (
  wmic Path win32_process Where "CommandLine Like '%%ShowHideWin.bat%%'" Call Terminate
)