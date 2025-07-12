module SmaLL1.Grammar

open SmaLL1.BasicTypes

let Empty =
    { Skips = []
      Terminals = []
      Rules = [] }

let withSkip s g = { g with Skips = s :: g.Skips }
let withTerminal t g = { g with Terminals = t :: g.Terminals }
let withRule r g = { g with Rules = r :: g.Rules }

let lex (skips: Pattern list) (terms: Terminal list) (input: string) =
    let skips = skips |> List.map Pattern.matcher

    // I hate imperative code either but here
    // it was just cleaner to write this way
    let l = input.Length
    let mutable pos = 0
    let mutable output = []

    while pos < l do
        let skipped = List.tryPick <| (|>) (input, pos) <| skips |> Option.map snd
        let parsed = List.tryPick <| Terminal.parsePrefix (input, pos) <| terms

        match skipped, parsed with
        | Some s, None -> pos <- s
        | None, Some(t, s) ->
            pos <- s
            output <- t :: output

        // TODO: add error handling as this is not a normal exit point for lexer
        | Some p, Some ({ Name = name }, _) -> failwith $"Token pattern matches skip pattern: {p}"
        | None, None -> failwith $"Could not parse the next token from position: {pos} \n\n Next symbol: {input[pos..]}"

    output
