#r "E:/git hub repo/Blog-Code/FSharpInterop/bin/Release/net9.0/FSharpInterop.dll"

open System
open FSharpInterop
open System.Reflection

let util = FSharpUtil()
util.Method()

let methodInfo = 
    typeof<FSharpUtil>.GetMethods() 
    |> Array.tryFind (fun m -> m.Name = "Method")

match methodInfo with
| Some m -> 
    for p in m.GetParameters() do
        printfn "Parameter: %s (Type: %A)" p.Name p.ParameterType
| None -> 
    printfn "Method not found."
