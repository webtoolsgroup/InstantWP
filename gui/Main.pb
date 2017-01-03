
;; Instant WordPress GUI
;; (c) Corvideon Ltd 2016
;; Seamus Brady <seamus@corvideon.ie>

;; Import "-stdlib=libc++ -mmacosx-version-min=10.7" : EndImport

XIncludeFile "MainWindow.pbf" ; Include the main window definition

OpenMainWindow() ; Open the first window

Global PWD$ = ProgramParameter(0)

;; set the current directory
SetCurrentDirectory(PWD$)

Procedure RunIWPCLI(Command$)
   If OSVersion() > #PB_OS_MacOSX_10_0
     ; OSX run command
     Ret = RunProgram(PWD$ + "/iwpcli", Command$, "", #PB_Program_Open)
   Else
     ; Windows
     MessageRequester("InstantWP", PWD$ + "/iwpcli.exe", #PB_MessageRequester_Ok | #PB_MessageRequester_Info)
     Ret = RunProgram(PWD$ + "/iwpcli.exe",  Command$, "", #PB_Program_Open)
   EndIf
   If Ret
    WaitProgram(Ret)
    CloseProgram(Ret)
  EndIf
EndProcedure

Procedure RunQuitCommand()

   If OSVersion() > #PB_OS_MacOSX_10_0
     ; OSX run command
     Ret = RunProgram(PWD$ + "/Quit-InstantWP", "", "", #PB_Program_Open)
   Else
     ; Windows
     Ret = RunProgram(PWD$ + "/Quit-InstantWP.bat", "", "", #PB_Program_Open)
   EndIf
   If Ret
    WaitProgram(Ret)
    CloseProgram(Ret)
  EndIf
EndProcedure

;; quit event
Procedure QuitGui()
  Result = MessageRequester("InstantWP", "Do you really want to quit InstantWP?", #PB_MessageRequester_YesNo)
  If Result = #PB_MessageRequester_Yes    ; pressed Yes button (Result = 6)
                                          ;; call the quit script here
    MessageRequester("InstantWP", "InstantWP is shutting down... please wait, this will take a few moments..", #PB_MessageRequester_Ok | #PB_MessageRequester_Info)
    RunQuitCommand()
    End ;; quit!
  EndIf
EndProcedure 
  
;; button events procs
Procedure WPFrontpageButtonEvent(EventType)
    RunIWPCLI("wpfrontpage")
EndProcedure
  
Procedure WPAdminButtonEvent(EventType)
    RunIWPCLI("wpadmin")
EndProcedure
  
Procedure WPPluginsButtonEvent(EventType)
    ;; RunIWPCLI("mount_webdav")
    RunIWPCLI("plugins")
EndProcedure
  
Procedure WPThemesButtonEvent(EventType)
    ; RunIWPCLI("mount_webdav")
    RunIWPCLI("themes")
EndProcedure  
  
Procedure MySQLButtonEvent(EventType)
    RunIWPCLI("mysql")
EndProcedure  
  
Procedure DocsButtonEvent(EventType)
  RunIWPCLI("docs")
EndProcedure  
  
Procedure AboutButtonEvent(EventType)
  RunIWPCLI("about")
EndProcedure 
  
Procedure QuitButtonEvent(EventType)
  QuitGui()
EndProcedure 


;; start IWP
;; RunIWPCLI("start")

; The main event loop
Repeat
  Event = WaitWindowEvent()
  
  Select EventWindow()
    Case MainWindow
      MainWindow_Events(Event) ; This procedure name is always window name followed by '_Events'
      
  EndSelect
  
Until Event = #PB_Event_CloseWindow ; Quit on any window close
                                    
QuitGui()
; IDE Options = PureBasic 5.50 (Windows - x86)
; CursorPosition = 22
; FirstLine = 9
; Folding = --
; EnableXP
; EnableUnicode