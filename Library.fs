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

type Terminal = { Name: string; Pattern: Pattern }

module Terminal =
    let RegexTerminal n r = { Name = n; Pattern = RegExp r }
    let SimpleTerminal n s = { Name = n; Pattern = Simple s }
    let CustomTerminal n f = { Name = n; Pattern = Custom f }

    let matcher t = Pattern.matcher t.Pattern

type Rule = { Name: string; Body: string list }

type Grammar =
    { Skips: Pattern list
      Terminals: Terminal list
      Rules: Rule list }

type Token = { Name: string; Value: string }

module Grammar =
    let Empty =
        { Skips = []
          Terminals = []
          Rules = [] }

    let withSkip s g = { g with Skips = s :: g.Skips }
    let withTerminal t g = { g with Terminals = t :: g.Terminals }
    let withRule r g = { g with Rules = r :: g.Rules }

    let lex (skips: Pattern list) (terms: Terminal list) (input: string) =
        let skips = skips |> List.map (fun pat -> { Name = ""; Pattern = pat })

        let rec helper leftover (output: Token list) =
            if leftover = "" then
                output
            else
                let rec iterator =
                    function
                    | [] -> None
                    | t :: xs ->
                        let f = Terminal.matcher t

                        match f leftover with
                        | None -> iterator xs
                        | Some(value, leftover) -> Some({ Name = t.Name; Value = value }, leftover)

                match iterator skips with
                | None ->
                    match iterator terms with
                    // TODO: add error handling as this is not normal exit point for lexer
                    | None -> output
                    | Some(t, leftover) -> helper leftover <| t :: output
                | Some(_, leftover) -> helper leftover output

        helper input []
