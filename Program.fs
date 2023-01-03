open day24
open day24.SnowField
open day24.PathFinder
let infinite = System.Int32.MaxValue - 10

let input = Input.readFile "/tmp/aoc/input.24.t2" 

type TimePos = int * int * int


// seq { 0 .. 10 } |> Seq.map (fun i -> leftByTime 5 i (4,3)) |> Seq.toList |> List.map (printfn "%A")

let sol1 () =
    let startPos = Set.singleton (0, fst input.Start, snd input.Start)
    let startState = CurrState(initialSnowField input,input.Goal,0,initialSnowField input,startPos)
    findSolution input.Goal startState 

// let sol2 () : int =
//     let reachedGoalAt =
//         findSolution input.Goal (initialSnowField input) 0 (Set.singleton (0, fst input.Start, snd input.Start))
//
//     let returnTime = reachedGoalAt+1
//     let returnPos = Set.singleton (returnTime, fst input.Goal, snd input.Goal)
//     let backAtStart = findSolution input.Start (initialSnowField input) returnTime returnPos
//     let backAtStart = backAtStart - 2
//     let secondTime = reachedGoalAt + backAtStart 
//     let secondTime = secondTime - 2
//
//     let atGoalAgain =
//         findSolution
//             input.Goal
//             (initialSnowField input)
//             secondTime
//             (Set.singleton (secondTime % (relevantTime input + 1), fst input.Start, snd input.Start))
//     // printfn $"sol2 {t1} {t2} {t3}"
//     printfn $"ReturnTime={reachedGoalAt} backAtStart={backAtStart} atGoalAgain={atGoalAgain}"
//     reachedGoalAt + backAtStart + atGoalAgain - 2
//
// let ts = seq { 0 .. relevantTime input * 10 } |> Seq.toList
//
// let startSnow (t) = (Set.singleton (t,fst input.Start, snd input.Start + 1))
// let endSnow (t) = (Set.singleton (t,fst input.Goal, snd input.Goal - 1))
//
// let tr =
//     ts
//     |> List.map (fun t -> findSolution input.Goal (initialSnowField input) t (startSnow t))
//
// let t2 =
//     ts
//     |> List.map (fun t -> findSolution input.Start (initialSnowField input) t (endSnow t))
//
//
// tr |> List.indexed |> List.take 5 |> List.map (printf "[%A]") 
// printfn ""
// t2 |> List.indexed |> List.skip 16 |> List.take 10 |> List.map (printf "[%A]")
// printfn ""
// tr |> List.indexed |> List.skip 40 |> List.take 10 |> List.map (printf "[%A]")
// printfn ""
//
printfn $"Sol={sol1 ()}"
