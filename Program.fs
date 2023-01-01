open System.IO
let infinite = System.Int32.MaxValue - 10

let rec gcd x y = if y = 0 then x else gcd y (x % y)
let input = File.ReadAllLines "/tmp/aoc/input.24.t2" |> Seq.toList

type TimePos = int * int * int

let startpos = input.Head.IndexOf '.' - 1, -1
let endpos = input[ input.Length - 1 ].IndexOf '.' - 1, (input.Length - 2)
let width = input.Head.Length - 2
let height = input.Length - 2
printfn $"start: {startpos} end={endpos} {width}x{height}"

let toPos (y: int) (line: string) (c: char) : Set<int * int> =
    line.ToCharArray()
    |> Array.indexed
    |> Array.filter (fun (_, n) -> n = c)
    |> Array.map (fun (x, _) -> x - 1, y - 1)
    |> Set.ofArray

let toPoss (c: char) (lines: string list) : Set<int * int> =
    lines
    |> List.indexed
    |> List.map (fun (y, line) -> toPos y line c)
    |> Set.unionMany

let lefts = input |> toPoss '<'
let rights = input |> toPoss '>'
let souths = input |> toPoss 'v'
let norths = input |> toPoss '^'

printfn $"lefts={lefts} rights={rights} norths={norths} souths={souths}"

let southByTime (h: int) (t: int) ((x, y): int * int) = x, (y + t) % h
let northByTime (h: int) (t: int) ((x, y): int * int) = x, (y - (t % h) + h) % h
let rightByTime (w: int) (t: int) ((x, y): int * int) = (x + t) % w, y
let leftByTime (w: int) (t: int) ((x, y): int * int) = (x - (t % w) + w) % w, y

// seq { 0 .. 10 } |> Seq.map (fun i -> leftByTime 5 i (4,3)) |> Seq.toList |> List.map (printfn "%A")

let relevantTime =
    let g = gcd height width
    let h = height / g
    let w = width / g
    g * h * w

printfn $"relevantTime: {relevantTime}"

let allPos =
    let ys = seq { 0 .. height - 1 } |> Seq.toList
    let xs = seq { 0 .. width - 1 } |> Seq.toList
    ys |> List.collect (fun y -> xs |> List.map (fun x -> x, y))

let freeSpacesAtT (t: int) =
    let lefts = lefts |> Set.map (leftByTime width t)
    let rights = rights |> Set.map (rightByTime width t)
    let norths = norths |> Set.map (northByTime height t)
    let souths = souths |> Set.map (southByTime height t)
    let taken = [ lefts; rights; norths; souths ] |> Set.unionMany
    let fieldPos = allPos |> List.filter (fun pos -> taken.Contains pos |> not)
    startpos :: endpos :: fieldPos


type SnowField = Map<TimePos, int>

let initialSnowFieldRaw: SnowField =
    let snowField: SnowField =
        seq { 0..relevantTime }
        |> Seq.collect (fun t -> freeSpacesAtT t |> List.map (fun (x, y) -> t, x, y))
        |> Seq.map (fun p -> p, infinite)
        |> Map.ofSeq

    snowField.Add((0, fst startpos, snd startpos), 0)

let surr (x, y) =
    [ (x, y - 1); (x - 1, y); (x, y); (x + 1, y); (x, y + 1) ]

let prevMap1: Map<TimePos, TimePos list> =
    printfn "Building prevmap"

    let prevs ((t, x, y): TimePos) =
        let t = (t + relevantTime) % (relevantTime + 1)
        let prevs = surr (x, y) |> List.map (fun (x, y) -> t, x, y)
        prevs |> List.filter initialSnowFieldRaw.ContainsKey

    initialSnowFieldRaw.Keys |> Seq.map (fun k -> k, prevs k) |> Map.ofSeq

let removeUnreachable (field: SnowField) =
    printfn "Removing unreachable"
    let reachable ((t, x, y): TimePos, _) =
        let prevs = prevMap1[(t, x, y)] |> List.filter field.ContainsKey
        prevs.IsEmpty |> not

    field |> Map.toSeq |> Seq.filter reachable |> Map.ofSeq

let rec removeUnreachableN (field: SnowField) =
    let pre = field.Count
    printfn $"removeUnreachableN (n={pre} field size={field.Count}"
    let field = removeUnreachable field
    let post = field.Count 
    if pre = post then field else removeUnreachableN field 

let initialSnowField = initialSnowFieldRaw |> removeUnreachableN

let prevMap: Map<TimePos, TimePos list> =
    printfn "Building prevmap"

    let prevs ((t, x, y): TimePos) =
        let t = (t + relevantTime) % (relevantTime + 1)
        let prevs = surr (x, y) |> List.map (fun (x, y) -> t, x, y)
        prevs |> List.filter initialSnowField.ContainsKey

    initialSnowFieldRaw.Keys |> Seq.map (fun k -> k, prevs k) |> Map.ofSeq

let bestTime (field: SnowField) ((t, x, y), curr) =
    let pos = (t, x, y)
    let prevs = prevMap[(t, x, y)]

    let best =
        prevs
        |> List.map (fun p -> field.TryFind p)
        |> List.filter Option.isSome
        |> List.map Option.get
        |> fun l -> if l.IsEmpty then [ infinite ] else l
        |> List.min

    let best = best + 1
    // printfn $"bestTime {(curr,best)}"
    pos, (min best curr)

let removeMinValue (field:SnowField) : SnowField =
    printfn "Removing min value"
    let m = field.Values |> Seq.min
    field |> Map.toSeq |> Seq.filter (fun ((t,x,y),v) -> ( (x,y) = endpos || v > m))|> Map.ofSeq 

let updateField (field: SnowField) =
    field |> Map.toSeq |> Seq.map (bestTime field) |> Map.ofSeq

let rec improveIters (iter: int) (field: SnowField) =
    printfn $"Improve: iter = {iter}"

    if iter = 0 then
        field
    else
        field |> updateField |> removeMinValue |> improveIters (iter - 1)

let field = initialSnowField |> removeUnreachableN |> improveIters 1000

let findBestEndPos (field: SnowField) : int =
    field
    |> Map.toSeq
    |> Seq.filter (fun ((_, x, y), _) -> (x, y) = endpos)
    |> Seq.map snd
    |> Seq.min

// field |> Map.toSeq |> Seq.toList |> List.map (printfn "FIELD: %A")
field |> findBestEndPos |> printfn "%A"

let printMapAt (t: int) =
    let lefts = lefts |> Set.map (leftByTime width t)
    let rights = rights |> Set.map (rightByTime width t)
    let norths = norths |> Set.map (northByTime height t)
    let souths = souths |> Set.map (southByTime height t)

    let toSym (x, y) =
        let l = if lefts.Contains(x, y) then '<' else '.'
        let r = if rights.Contains(x, y) then '>' else '.'
        let n = if norths.Contains(x, y) then '^' else '.'
        let s = if souths.Contains(x, y) then 'v' else '.'

        match [ l; r; n; s ] |> List.filter (fun c -> c <> '.') with
        | [] -> '.'
        | [ c ] -> c
        | l -> l.Length |> char |> (fun c -> c + '0')


    let ys = seq { 0 .. height - 1 } |> Seq.toList
    let xs = seq { 0 .. width - 1 } |> Seq.toList

    ys
    |> List.map (fun y ->
        let lps = xs |> List.map (fun x -> x, y) |> List.map toSym
        lps |> List.map (printf "%c")
        printfn "")

// printfn "5:"
// printMapAt 5

// prevMap |> Map.toSeq |> Seq.toList |> List.map (printfn "%A")
