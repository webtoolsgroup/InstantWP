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
  
  ;; sftp path
  sftp.s = currDir.s + "platform\win\sftp\WinSCP.exe"
  
  If OpenConsole()
    PrintN("Running the SFTP client at " + sftp.s )
  EndIf

 
  RunProgram(sftp.s, "",  currDir.s )
  
EndProcedure

Main()
; IDE Options = PureBasic 5.61 (Windows - x86)
; ExecutableFormat = Console
; CursorPosition = 23
; Folding = -
; EnableXP
; Executable = start-SFTP.exe