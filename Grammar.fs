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
    let mutable leftover = input
    let mutable output = []

    while leftover <> "" do
        let skipped = List.tryPick <| (|>) leftover <| skips |> Option.map snd
        let parsed = List.tryPick <| Terminal.parsePrefix leftover <| terms

        match skipped, parsed with
        | Some s, None -> leftover <- s
        | None, Some(t, s) ->
            leftover <- s
            output <- t :: output

        // TODO: add error handling as this is not normal exit point for lexer
        | Some _, Some _ -> failwith "Token pattern matches skip pattern"
        | None, None -> failwith $"Could not parse the next token from: {leftover} \n\n Next symbol: {leftover[0]}"

    output
