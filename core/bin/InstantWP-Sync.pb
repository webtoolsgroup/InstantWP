
;; InstantWP WinSCP Starting tool
;; (c) Corvideon 2017

;; Current directory
CurrDir.s = GetPathPart(ProgramFilename())

;; ini file
iniFile.s = CurrDir.s + "\sync.ini"

;; Params
OpenPreferences(iniFile)
PreferenceGroup("FileSync")
UseSync.s = ReadPreferenceString("UseSync", "")
SyncScript.s = ReadPreferenceString("SyncScript", "")
SyncUser.s = ReadPreferenceString("SyncUser", "")
SyncPassword.s = ReadPreferenceString("SyncPassword", "")
SyncPort.s = ReadPreferenceString("SyncPort", "")
SyncHost.s = ReadPreferenceString("SyncHost", "")
SyncLocalRoot.s = ReadPreferenceString("SyncLocalRoot", "")
SyncVMRoot.s = ReadPreferenceString("SyncVMRoot", "")
SyncPIDFile.s = ReadPreferenceString("SyncPIDFile", "")
SyncTimerMilliseconds.s = ReadPreferenceString("SyncTimerMilliseconds", "")
UseSyncLoop.s = ReadPreferenceString("UseSyncLoop", "")
ClosePreferences()


;; stop execution if no syncing
If UseSync.s="no"
  End
EndIf

;; create a new PID file
PidFile.s = CurrDir.s + "\" + SyncPIDFile
DeleteFile(PidFile.s)
PID.l = GetCurrentProcessId_()
If OpenFile(0, PidFile.s)    
   WriteStringN(0, Str(PID.l))
   CloseFile(0)
EndIf

;; Switch directory to local sync dir
SetCurrentDirectory(CurrDir.s + "\..\..\..\")
tmpDir.s = GetCurrentDirectory()
SetCurrentDirectory(tmpDir.s + "\" + SyncLocalRoot.s)

;; get the full path to the local sync dir
FullSyncLocalRoot.s = GetCurrentDirectory()

;; reset the current directory
SetCurrentDirectory(CurrDir.s)

;; WinSCP path
winSCP.s = CurrDir.s + "\winSCP.com"
;; script path
scriptPath.s = CurrDir.s + SyncScript
;; run WinSCP
winscpParams.s = "/script=" + scriptPath.s + " /parameter " + SyncUser.s + " " + SyncPassword.s  + " " + SyncHost.s  + " " + SyncPort.s  + " " + FullSyncLocalRoot.s  + " " + SyncVMRoot.s + " /log=InstantWP-sync.log" 

;; don't use loop, just kickoff sync and exit
If(UseSyncLoop="no")
  runReturn.d = RunProgram(winSCP.s, winscpParams.s, "", #PB_Program_Open)
  End ;; exit the program
EndIf

;; Main program loop
Repeat
  runReturn.d = RunProgram(winSCP.s, winscpParams.s, "", #PB_Program_Open)
  Delay(ValD(SyncTimerMilliseconds.s))
ForEver

; IDE Options = PureBasic 5.50 (Windows - x86)
; ExecutableFormat = Console
; CursorPosition = 64
; FirstLine = 38
; EnableXP
; Executable = ..\platform\win\sync\InstantWP-Sync.exe