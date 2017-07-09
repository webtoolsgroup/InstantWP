
REM Starts a batch file in the background...

If WScript.Arguments.Count >= 1 Then
    ReDim arr(WScript.Arguments.Count-1)
    For i = 0 To WScript.Arguments.Count-1
        Arg = WScript.Arguments(i)
        If InStr(Arg, " ") > 0 Then Arg = """" & Arg & """"
      arr(i) = Arg
    Next

    RunCmd = Join(arr)
    CreateObject("Wscript.Shell").Run RunCmd, 0, True
End If