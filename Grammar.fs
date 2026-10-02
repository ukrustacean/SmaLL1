module SmaLL1.Grammar

open System.Linq
open SmaLL1.BasicTypes

let Empty =
    { Skips = []
      Terminals = []
      Rules = [] }

let withSkip s g = { g with Skips = s :: g.Skips }
let withTerminal t g = { g with Terminals = t :: g.Terminals }
let withRule r g = { g with Rules = r :: g.Rules }

let withSkips s g = { g with Skips = List.ofSeq <| g.Skips.Concat(s) }
let withTerminals t g = { g with Terminals = List.ofSeq <| g.Terminals.Concat(t) }
let withRules r g = { g with Rules = List.ofSeq <| g.Rules.Concat(r) }


let lex (input: string) (g: Grammar) =
    let skips = g.Skips |> List.map Pattern.matcher

    // I hate imperative code either but here
    // it was just cleaner to write this way
    let l = input.Length
    let mutable pos = 0
    let mutable output = []

    while pos < l do
        let skipped = List.tryPick <| (|>) (input, pos) <| skips |> Option.map snd
        let parsed = List.tryPick <| Terminal.parsePrefix (input, pos) <| g.Terminals

        match skipped, parsed with
        | Some s, None -> pos <- s
        | None, Some(t, s) ->
            pos <- s
            output <- t :: output

        // TODO: add error handling as this is not a normal exit point for lexer
        | Some p, Some ({ Name = name }, _) -> failwith $"Token pattern `{name}` matches skip pattern `{p}`"
        | None, None -> failwith $"Could not parse the next token from position: {pos} \n\n Next symbol: {input[pos..]}"

    output
