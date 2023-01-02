open System.IO
let infinite = System.Int32.MaxValue - 10

let rec gcd x y = if y = 0 then x else gcd y (x % y)
let input = File.ReadAllLines "/tmp/aoc/input" |> Seq.toList

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


type SnowField = Set<TimePos>

let initialSnowField: SnowField =
    seq { 0..relevantTime }
    |> Seq.collect (fun t -> freeSpacesAtT t |> List.map (fun (x, y) -> t, x, y))
    |> Set.ofSeq


let rec findSolution (field: SnowField) (time: int) (currentlyAt: Set<TimePos>) : int =
    if (time % 10 = 0) then printfn $"findSolution field:{field.Count} time={time} currentlyAt: {currentlyAt.Count}"
    let foundEnd =
        currentlyAt
        |> Set.filter (fun (t, x, y) -> (x, y) = endpos)
        |> Set.isEmpty
        |> not

    if foundEnd then
        0
    else

        let next (t, x, y) =
            let t = (time + 1) % (relevantTime + 1)
            let n = [ (t, x, y - 1); (t, x - 1, y); (t, x, y); (t, x + 1, y); (t, x, y + 1) ]
            // printfn $"next({t},{x}.{y} = {n}"
            n 
        let nexts = currentlyAt |> Set.toList |> List.collect next |> Set.ofList |> Set.intersect field
        let field = field |> Set.filter (fun pos -> nexts.Contains pos |> not)
        1 + findSolution field (time+1) nexts 

let sol = findSolution initialSnowField 0 (Set.singleton (0,fst startpos,snd startpos))

printfn $"Sol={sol}"