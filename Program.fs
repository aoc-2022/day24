open day24
open day24.SnowField
open day24.PathFinder

let infinite = System.Int32.MaxValue - 10

let input = Input.readFile "/tmp/aoc/input"

type TimePos = int * int * int


// seq { 0 .. 10 } |> Seq.map (fun i -> leftByTime 5 i (4,3)) |> Seq.toList |> List.map (printfn "%A")

let sol1 () =
    let startPos = Set.singleton (0, fst input.Start, snd input.Start)

    let startState =
        CurrState(initialSnowField input, input.Goal, 0, initialSnowField input, startPos)

    findSolution startState

let sol2 () : int =
    let southOf (t,x,y) = (t,x,y+1)
    let northOf (t,x,y) = (t,x,y-1)
    let field = initialSnowField input
    let startPos = Set.singleton (0, fst input.Start, snd input.Start)
    let startState = CurrState(field, input.Goal, 0, field, startPos)
    let t1, endPos1 = findSolution startState
    let startState = CurrState(field, input.Start, 0, field, endPos1 |> Set.singleton)
    let t2, startPos2 = findSolution startState
    let startState = CurrState(field, input.Goal, 0, field, startPos2 |> Set.singleton)
    let t3, endPos2 = findSolution startState
    printfn $"sol2: startPos = {t1}:{startPos} endPos1={t2}:{endPos1} startPos2={t3}:{startPos2} endPos2={t1+t2+t3}:{endPos2}"
    (t1+t2+t3)

printfn $"Sol={sol2 ()}"
