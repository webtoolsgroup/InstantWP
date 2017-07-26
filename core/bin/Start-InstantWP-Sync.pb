;; InstantWP Sync Starting tool
;; (c) Corvideon 2017

;; Current directory
CurrDir.s = GetCurrentDirectory()

;; Start the exe to loop winscp
looper.s = CurrDir.s + "\InstantWP-Sync.exe"

RunProgram(looper.s, "", "", #PB_Program_Open | #PB_Program_Hide)
; IDE Options = PureBasic 5.50 (Windows - x86)
; CursorPosition = 9
; EnableXP
; Executable = Start-InstantWP-Sync.exe