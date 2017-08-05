Import "-stdlib=libc++ -mmacosx-version-min=10.7" : EndImport

;; Instant WordPress Commandline Interface
;; (c) Corvideon Ltd 2016
;; Seamus Brady <seamus@corvideon.ie>

Command$ = ProgramParameter(0)
If Command$ = "" 
  PrintN("InstantWP (IWP) commandline interface options:")                                                                
  PrintN("iwpcli status")                                           
  PrintN("              Check if IWP is running")                   
  PrintN("iwpcli start")                                            
  PrintN("              Start InstantWP")                           
  PrintN("iwpcli quit")                                             
  PrintN("              Quit InstantWP")                            
  PrintN("iwpcli wpfrontpage")                                      
  PrintN("              Open WordPress frontpage")                  
  PrintN("iwpcli wpadmin")                                          
  PrintN("              Open WordPress Dashboard")                  
  PrintN("iwpcli plugins")                                          
  PrintN("              Open the plugins folder")                   
  PrintN("iwpcli themes")                                           
  PrintN("              Open the themes folder")                   
  PrintN("iwpcli mysql")                                            
  PrintN("              Open PHPMyAdmin")                           
  PrintN("iwpcli docs")                                             
  PrintN("              Open IWP documentation")     
  PrintN("iwpcli about")                                             
  PrintN("              Open IWP about page")   
  PrintN("iwpcli ssh")                                              
  PrintN("              Open up an SSH session on the IWP Linux VM")
  PrintN("iwpcli monitor")                                          
  PrintN("              Open up the QEMU VM Monitor shell")    
  PrintN("iwpcli webconsole")                                          
  PrintN("              Open up the Instant Web Console")  
  End
EndIf  
IWPRoot$ = GetCurrentDirectory()
OpenConsole()
If OpenConsole()
  PrintN("The InstantWP root folder is " + IWPRoot$)
  If OSVersion() > #PB_OS_MacOSX_10_0
    ; OSX run command
     Ret = RunProgram(IWPRoot$ + "/bin/iwp",  "--iwproot " + IWPRoot$ + " --" + Command$, "", #PB_Program_Open)
   Else
     ; Windows
     Ret = RunProgram(IWPRoot$ + "iwp.exe",  "--iwproot " + IWPRoot$ + " --" + Command$, "", #PB_Program_Open)
   EndIf
  If Ret
    WaitProgram(Ret)
    CloseProgram(Ret)
  EndIf
EndIf
CloseConsole()
End
; IDE Options = PureBasic 5.50 (MacOS X - x64)
; ExecutableFormat = Console
; CursorPosition = 34
; FirstLine = 9
; EnableXP
; Executable = iwpcli
; DisableDebugger
; EnableUnicode