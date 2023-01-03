module day24.PathFinder

open day24.Common
open day24.SnowField

type CurrState(field: SnowField, goal: Pos, time: int, unexplored: SnowField, pos: Set<TimePos>) =
    member this.Field = field
    member this.Goal = goal
    member this.Time = time
    member this.Unexplored = unexplored
    member this.Pos = pos

    member this.IsAtEnd =
        let atEnd ((_, x, y): TimePos) = (x, y) = goal
        pos |> Set.filter atEnd |> Set.isEmpty |> not
    member this.End =
        let atEnd ((_, x, y): TimePos) = (x, y) = goal
        pos |> Set.filter atEnd |> Set.minElement
        

let rec findSolution (state: CurrState) : int*TimePos =
    if state.IsAtEnd then
        state.Time,state.End 
    else

        let next (t, x, y) =
            let t = (t + 1) % (state.Field.Timespan )
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
        findSolution nextState
