open Microsoft.FSharp.Math


type Cell = { x:int; y:int;}

let SN = 9995

let GetRackID (c: Cell): int = 
    c.x + 10

let GetHundredDigit (n: int) =
    let s = n.ToString()
    match s with 
        | s when s.Length < 3 -> 0
        | _ -> s.[s.Length-3..s.Length-3] |> int

let minus5 x = x - 5

let CalcPowerLevel (c: Cell) = 
    let power = 
        (GetRackID c * c.y)
        |> (+) SN
        |> (*) (GetRackID c)
        |> GetHundredDigit
        |> minus5
    power

let CalcAreaPower (c: Cell) =
    Seq.sum <| seq {
        for x in c.x..c.x+2 do
        for y in c.y..c.y+2 do
        yield CalcPowerLevel {x=x; y=y}
    }

seq {
  for x in 1..300 do
  for y in 1..300 do
  yield {x=x; y=y}
}
    |> Seq.maxBy CalcAreaPower
    |> printfn "%A"

// printfn "%i" (CalcPowerLevel {x=217;y=196})
// printfn "%A" (CalcAreaPower {x=33;y=45})
