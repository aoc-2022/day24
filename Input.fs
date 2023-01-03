module day24.Input

open System.IO
open day24.Common

type Input
    (
        rights: Set<Pos>,
        lefts: Set<Pos>,
        norths: Set<Pos>,
        souths: Set<Pos>,
        start: Pos,
        goal: Pos,
        width: int,
        height: int
    ) =
    member this.Rights = rights
    member this.Lefts = lefts
    member this.Norths = norths
    member this.Souths = souths
    member this.Start = start
    member this.Goal = goal
    member this.Width = width
    member this.Height = height
    member this.Foo = 1


let private toPos (y: int) (line: string) (c: char) : Set<int * int> =
    line.ToCharArray()
    |> Array.indexed
    |> Array.filter (fun (_, n) -> n = c)
    |> Array.map (fun (x, _) -> x - 1, y - 1)
    |> Set.ofArray

let private toPoss (c: char) (lines: string list) : Set<int * int> =
    lines
    |> List.indexed
    |> List.map (fun (y, line) -> toPos y line c)
    |> Set.unionMany


let readFile (fileName: string) =
    let input = File.ReadAllLines fileName |> Seq.toList
    let startpos = input.Head.IndexOf '.' - 1, -1
    let endpos = input[ input.Length - 1 ].IndexOf '.' - 1, (input.Length - 2)
    let width = input.Head.Length - 2
    let height = input.Length - 2
    let lefts = input |> toPoss '<'
    let rights = input |> toPoss '>'
    let souths = input |> toPoss 'v'
    let norths = input |> toPoss '^'
    Input(rights, lefts, norths, souths, startpos, endpos, width, height)
