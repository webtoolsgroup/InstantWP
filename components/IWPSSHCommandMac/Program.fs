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
        
     let mutable username = ""
     let mutable password = "" 
     let mutable port = ""
     let mutable command = ""

     let runSSHCommand port user pass command =
         let ssh = new Ssh()

         // connect and log on
         ssh.Connect(localhost, System.Int32.Parse(port))

         ssh.Login(username, password)

         // start a scripting session
         let scripting = ssh.StartScripting()

         // automatically detect remote prompt
         scripting.DetectPrompt();

         // execute command
         scripting.SendCommand(command)

         // read its response
         let response = scripting.ReadUntilPrompt()
        
         // display the response
         Console.WriteLine(response);
   
     [<EntryPoint>]
     let main args =

        let arglist = args |> List.ofSeq

        match arglist with
            | [first; second; third; fourth;] -> 
                    port <- first
                    username <- second
                    password <- third
                    command <- fourth
            | _ -> 
                    printfn "Instant WordPress SSH Command Client"
                    System.Environment.Exit(1)

        runSSHCommand port username password command

        // exit
        0