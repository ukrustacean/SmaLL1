module SmaLL1.BasicTypes

type Matcher = string -> (string * string) option

type Pattern =
    | RegExp of string
    | Simple of string
    | Custom of Matcher

type Token = { Name: string; Value: string }

type Terminal = { Name: string; Matcher: Matcher }

type Rule = { Name: string; Body: string list }

type Grammar =
    { Skips: Pattern list
      Terminals: Terminal list
      Rules: Rule list }
