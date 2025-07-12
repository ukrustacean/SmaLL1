module SmaLL1.BasicTypes

open System.Text.RegularExpressions

type Matcher = string * int -> (string * int) option

type Pattern =
    | RegExp of Regex
    | Simple of string
    | Custom of Matcher

type Token = { Name: string; Value: string }

type Terminal = { Name: string; Matcher: Matcher }

type Rule = { Name: string; Body: string list }

type Grammar =
    { Skips: Pattern list
      Terminals: Terminal list
      Rules: Rule list }
