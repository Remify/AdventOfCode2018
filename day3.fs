open System.Text.RegularExpressions
open System.Collections.Generic

let input = "#1297 @ 937,175: 16x3
#1298 @ 794,307: 10x20
#1299 @ 509,299: 16x19"

type Claim = {X: int; Y: int;W: int; H: int}

let ParseToClaim (str: string): Claim = 
     let m = Regex.Match(str, @"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$")
     let values = List.tail [ for g in m.Groups -> g.Value ]
     let x, y = int(values.Item 1), int(values.Item 2)
     let w, h = int(values.Item 3), int(values.Item 4)
     let record = { X=x; Y=y; W=w; H=h }
     record

let FindOverlaps (input: Claim []) = 
    let dict = new Dictionary<int * int, bool>();
    let mutable count = 0
    for c in input do
     for i in [|c.X..(c.X + c.W - 1)|] do
          for j in [|c.Y..(c.Y + c.H - 1)|] do
               let key = (i, j)
               if (dict.ContainsKey key) then
                    let isTwice = dict.Item key
                    if not(isTwice) then
                         count <- count + 1
                         dict.Item key <- true
               else 
                    dict.Add(key, false)
    count


let overlapsCount = FindOverlaps (input.Split '\n' |> Array.map ParseToClaim)
