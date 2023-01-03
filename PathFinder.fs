module day24.PathFinder

open day24.Common
open day24.Input
open day24.SnowField

type CurrState(field: SnowField, goal: Pos, time: int, unexplored: SnowField, pos: Set<TimePos>) =
    member this.Field = field
    member this.Goal = goal
    member this.Time = time
    member this.Unexplored = unexplored
    member this.Pos = pos

    member this.IsAtEnd =
        let atEnd ((x, y, z): TimePos) = (x, y) = goal
        pos |> Set.filter atEnd |> Set.isEmpty |> not

let rec findSolution (endpos: int * int) (state: CurrState) : int =
    let foundEnd =
        state.Pos |> Set.filter (fun (_, x, y) -> (x, y) = endpos) |> Set.isEmpty |> not

    if foundEnd then
        0
    else

        let next (t, x, y) =
            let t = (t + 1) % (state.Field.Timespan + 1)
            let n = [ (t, x, y - 1); (t, x - 1, y); (t, x, y); (t, x + 1, y); (t, x, y + 1) ]
            // printfn $"next({t},{x}.{y} = {n}"
            n

        let nexts =
            state.Pos
            |> Set.toList
            |> List.collect next
            |> Set.ofList
            |> Set.intersect state.Unexplored.Field

        let field = state.Unexplored.Remove nexts
        let nextState = CurrState(state.Field, state.Goal, state.Time + 1, field, nexts)
        1 + findSolution endpos nextState
