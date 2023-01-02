module day24.SnowField


open day24.Input
open day24.Common

type SnowField (field: Set<int*int*int>) =
    member this.Field = field 

let relevantTime (input:Input) =
    let g = gcd input.Height input.Width
    let h = input.Height / g
    let w = input.Width / g
    g * h * w

printfn $"relevantTime: {relevantTime}"

let southByTime (h: int) (t: int) ((x, y): int * int) = x, (y + t) % h
let northByTime (h: int) (t: int) ((x, y): int * int) = x, (y - (t % h) + h) % h
let rightByTime (w: int) (t: int) ((x, y): int * int) = (x + t) % w, y
let leftByTime (w: int) (t: int) ((x, y): int * int) = (x - (t % w) + w) % w, y

let allPos (input:Input) =
    let ys = seq { 0 .. input.Height - 1 } |> Seq.toList
    let xs = seq { 0 .. input.Width - 1 } |> Seq.toList
    ys |> List.collect (fun y -> xs |> List.map (fun x -> x, y))


let freeSpacesAtT (input:Input) (t: int) =
    let lefts = input.Lefts |> Set.map (leftByTime input.Width t)
    let rights = input.Rights |> Set.map (rightByTime input.Width t)
    let norths = input.Norths |> Set.map (northByTime input.Height t)
    let souths = input.Souths |> Set.map (southByTime input.Height t)
    let taken = [ lefts; rights; norths; souths ] |> Set.unionMany
    let fieldPos = allPos input |> List.filter (fun pos -> taken.Contains pos |> not)
    input.Start :: input.Goal :: fieldPos  
    
let initialSnowField (input:Input): SnowField =
    seq { 0..(relevantTime input) }
    |> Seq.collect (fun t -> freeSpacesAtT input t |> List.map (fun (x, y) -> t, x, y))
    |> Set.ofSeq |> SnowField 

