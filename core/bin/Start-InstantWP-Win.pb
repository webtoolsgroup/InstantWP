;; Start InstantWP on Windows
;; (c) Corvideon 2017

;; Current directory
CurrDir.s = GetPathPart(ProgramFilename())

;; Switch directory to iwpcli dir
SetCurrentDirectory(CurrDir.s + "\..\")
iwpcliDir.s = GetCurrentDirectory()
runiwp.s = iwpcliDir.s + "iwpcli.exe"

;; Start up IWP!!
RunProgram(runiwp.s, "start", "", #PB_Program_Open | #PB_Program_Hide)
; IDE Options = PureBasic 5.50 (Windows - x86)
; ExecutableFormat = Console
; CursorPosition = 12
; EnableXP
; UseIcon = ..\images\IWP.ico
; Executable = C:\Users\paperspace\Dumpster\test-5.1.0-rc62\bin\Start-InstantWP-Win.exe