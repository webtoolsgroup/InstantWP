;; InstantWP QEMU window closing tool
;; (c) Corvideon 2017


Procedure FindPartWin(part$)
  r=GetWindow_(GetDesktopWindow_(),#GW_CHILD)
  Repeat
    t$=Space(999) : GetWindowText_(r,t$,999)
    If FindString(t$,part$,1)<>0
      w=r
    Else
      r=GetWindow_(r,#GW_HWNDNEXT)
    EndIf
  Until r=0 Or w<>0
  ProcedureReturn w
EndProcedure

Procedure Main()
  ;; Current directory
  CurrDir.s = GetPathPart(ProgramFilename())
  
  ;; ini file
  iniFile.s = CurrDir.s + "..\config\iwp-win.ini"
  
  ;; Params
  OpenPreferences(iniFile)
  PreferenceGroup("vmports")
  PortOffset.f = Val(ReadPreferenceString("PortOffset", "10000"))
  SSH.f = Val(ReadPreferenceString("SSH", "22"))
  ClosePreferences()
  
  ;; port number
  SSHPort.f = PortOffset.f + SSH.f
  
  ;; title of QEMU Window
  QEMUWindowTitle.s = "QEMU (IWPServer-" + Str(SSHPort.f) + ")"
  
  ;; find the QEMU window
  hWnd =  FindPartWin(QEMUWindowTitle.s)
  
  ;; hide the window
  ShowWindow_(hWnd, #SW_HIDE)
EndProcedure

Main()


; IDE Options = PureBasic 5.50 (Windows - x86)
; ExecutableFormat = Console
; CursorPosition = 38
; FirstLine = 12
; Folding = -
; EnableXP
; Executable = HideQEMUWindow.exe