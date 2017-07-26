
;; InstantWP WinSCP Starting tool
;; (c) Corvideon 2017

;; Current directory
CurrDir.s = GetCurrentDirectory()

;; ini file
iniFile.s = CurrDir.s + "\sync.ini"

;; Params
OpenPreferences(iniFile)
PreferenceGroup("FileSync")
SyncUser.s = ReadPreferenceString("SyncUser", "")
SyncPassword.s = ReadPreferenceString("SyncPassword", "")
SyncPort.s = ReadPreferenceString("SyncPort", "")
SyncHost.s = ReadPreferenceString("SyncHost", "")
SyncLocalRoot.s = ReadPreferenceString("SyncLocalRoot", "")
SyncVMRoot.s = ReadPreferenceString("SyncVMRoot", "")
SyncPIDFile.s = ReadPreferenceString("SyncPIDFile", "")
SyncTimerMilliseconds.s = ReadPreferenceString("SyncTimerMilliseconds", "")
ClosePreferences()

;; create a new PID file
PidFile.s = SyncPIDFile
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
;; run WinSCP
winscpParams.s = "/script=scp.script /parameter " + SyncUser.s + " " + SyncPassword.s  + " " + SyncHost.s  + " " + SyncPort.s  + " " + FullSyncLocalRoot.s  + " " + SyncVMRoot.s 

;; Main program loop
Repeat
  runReturn.d = RunProgram(winSCP.s, winscpParams.s, "", #PB_Program_Open | #PB_Program_Hide)
  Delay(ValD(SyncTimerMilliseconds.s))
ForEver

; IDE Options = PureBasic 5.50 (Windows - x86)
; ExecutableFormat = Console
; CursorPosition = 23
; FirstLine = 5
; EnableXP
; Executable = InstantWP-Sync.exe