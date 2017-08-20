
;; Instant WordPress Commandline Interface
;; (c) Corvideon Ltd 2016
;; Seamus Brady <seamus@corvideon.ie>

Command$ = ProgramParameter(0)
IWPRoot$ = GetCurrentDirectory()
OpenConsole()
If OpenConsole()
  PrintN("The InstantWP root folder is " + IWPRoot$)
  Ret = RunProgram(IWPRoot$ + "/bin/iwp.exe",  "--iwproot " + IWPRoot$ + " --" + Command$, "", #PB_Program_Open)
  If Ret
    WaitProgram(Ret)
    CloseProgram(Ret)
  EndIf
EndIf
CloseConsole()
End
; IDE Options = PureBasic 5.50 (Windows - x86)
; ExecutableFormat = Console
; CursorPosition = 5
; EnableXP
; Executable = iwpcli.exe
; DisableDebugger
; EnableUnicode