module SmaLL1.Program

open SmaLL1.Grammar
open SmaLL1.Terminal

let keyword s = SimpleTerminal s s

[<EntryPoint>]
let main _ =
    let skips = [ Simple " "; Simple "\t"; Simple "\n" ]

    let terminals =
        [ keyword "function"
          keyword "return"
          keyword "{"
          keyword "}"
          keyword "("
          keyword ")"
          keyword ";"
          keyword ","
          RegexTerminal "Number" "[0-9]+"
          RegexTerminal "Ident" "[_a-zA-Z][_a-zA-Z0-9]*" ]

    // let result = lex skips terminals "function helloWorld(a, b, c) { return 0; }"
    //
    // result
    // |> List.rev
    // |> List.iter (fun { Name = name; Value = value } -> printf $"`{value}` ")

    printfn ""

    0
