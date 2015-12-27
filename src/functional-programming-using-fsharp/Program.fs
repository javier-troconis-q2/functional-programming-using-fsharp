namespace functional_programming_using_fsharp

type Program () =
    member x.Main (argv: string array) =
        printfn "%A" argv
        0