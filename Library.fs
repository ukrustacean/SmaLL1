namespace SmaLL1

open System.Text.RegularExpressions

type Matcher = string -> (string * string) option

type Pattern =
    | RegExp of string
    | Simple of string
    | Custom of Matcher

module Pattern =
    let simpleMatch pat : Matcher =
        let rec helper a b =
            match (a, b) with
            | [], b -> Some b
            | a, [] -> Some a
            | x :: xs, y :: ys when x = y -> helper xs ys
            | _ -> None

        fun s ->
            helper <| List.ofSeq pat <| List.ofSeq s
            |> Option.map (fun x -> (pat, x |> List.toArray |> System.String))

    let regexpMatch regex : Matcher =
        let r = Regex <| "^" + regex

        fun s ->
            let v = r.Match(s).Value
            if v = "" then None else Some(v, r.Replace(s, ""))

    let customMatch f : Matcher = f

    let matcher =
        function
        | RegExp r -> regexpMatch r
        | Simple s -> simpleMatch s
        | Custom c -> customMatch c

    let skipPattern pat s =
        matcher pat s |> Option.map snd |> Option.defaultValue s

type Token = { Name: string; Value: string }

type Terminal = { Name: string; Matcher: Matcher }

module Terminal =
    let RegexTerminal n r =
        { Name = n
          Matcher = Pattern.matcher <| RegExp r }

    let SimpleTerminal n s =
        { Name = n
          Matcher = Pattern.matcher <| Simple s } //List.tryPick

    let CustomTerminal n f =
        { Name = n
          Matcher = Pattern.matcher <| Custom f }
        
    let parsePrefix s { Name = name; Matcher = f } =
        f s |> Option.map (fun (value, left) -> ({ Name = name; Value = value }, left))

type Rule = { Name: string; Body: string list }

type Grammar =
    { Skips: Pattern list
      Terminals: Terminal list
      Rules: Rule list }

module Grammar =
    let Empty =
        { Skips = []
          Terminals = []
          Rules = [] }

    let withSkip s g = { g with Skips = s :: g.Skips }
    let withTerminal t g = { g with Terminals = t :: g.Terminals }
    let withRule r g = { g with Rules = r :: g.Rules }

    let lex (skips: Pattern list) (terms: Terminal list) (input: string) =
        let skips = skips |> List.map (fun pat -> Pattern.matcher pat)

        let rec helper leftover (output: Token list) =
            if leftover = "" then
                output
            else
                let skipped = skips |> List.tryPick (fun f -> f leftover |> Option.map snd)
                let parsed = terms |> List.tryPick (Terminal.parsePrefix leftover)

                match skipped with
                | None ->
                    match parsed with
                    // TODO: add error handling as this is not normal exit point for lexer
                    | None -> output
                    | Some(t, leftover) -> helper leftover <| t :: output
                | Some s -> helper s output

        helper input []
