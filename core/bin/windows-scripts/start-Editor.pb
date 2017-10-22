;; InstantWP runcli
;; (c) Corvideon 2017


Procedure Main()
  ;; Current directory
  CurrDir.s = GetPathPart(ProgramFilename())
  
  ;; root dit
  rootDir.s = CurrDir.s + "..\..\..\"
  SetCurrentDirectory(rootDir.s)
  
  ; set the current dir
  currDir.s = GetCurrentDirectory()
  
  ;; config path
  config.s = currDir.s + "config\iwp-win.ini"
  
  
  If OpenConsole()
    PrintN("Opening the config file at " + config.s )
  EndIf

 
  RunProgram("notepad", config.s,  currDir.s )
  
EndProcedure

Main()
; IDE Options = PureBasic 5.61 (Windows - x86)
; ExecutableFormat = Console
; CursorPosition = 16
; Folding = -
; EnableXP
; Executable = start-Editor.exe