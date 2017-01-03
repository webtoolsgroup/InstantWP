// main program module
namespace InstantWordPress

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Text
open Rebex.Net
open Rebex.TerminalEmulation

module Program = 

     [<Literal>]
     let localhost = "127.0.0.1"
        
     let mutable port = ""
     let mutable command = ""

     let runTelnetCommand port command = 
         let client = new Telnet(localhost, System.Int32.Parse(port))
         let shell = client.StartShell()
         shell.Prompt <- "(qemu) "
         let response = shell.ReadAll("(qemu) ")
         shell.SendCommand(command)
         let response = shell.ReadAll("(qemu) ")
         Console.WriteLine(response)

   
     [<EntryPoint>]
     let main args =

        let arglist = args |> List.ofSeq

        match arglist with
            | [first; second;] -> 
                    port <- first
                    command <- second
            | _ -> 
                    printfn "Instant WordPress QEMU Telnet Client"
                    System.Environment.Exit(1)

        runTelnetCommand port command

        // exit
        0