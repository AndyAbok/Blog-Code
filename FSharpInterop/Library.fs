namespace FSharpInterop

type FSharpUtil() =

    member _.Method(?i: int) =
        let i = defaultArg i 42
        System.Console.WriteLine(i)


        