module day24.Common

type Pos = int*int
type TimePos = int*int*int


let rec gcd x y = if y = 0 then x else gcd y (x % y)
