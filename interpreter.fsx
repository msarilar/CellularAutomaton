(*
    LinqPad style for results:
    
    body
    {
        margin: 0.3em 0.3em 0.4em 0.4em;
        font-family: Consolas;
        font-size: 80%;
        background: white;
        line-height: 57%;
    }
*)

let random = new Random()

let size = 200
let ruleNumber = random.Next(0, 256)
ruleNumber.Dump();
"".Dump();
let iterations = size / 2

let buildBoolArray n =
    [0..n] |> Seq.map (fun idex -> match random.Next(0, 100) with
                                   | x when x >= 10 -> true
                                   | _ -> false)
           |> Seq.toArray

let computeRule number =
    let hasBit num bit =
        num &&& (1 <<< bit) > 0
    let toBool n =
        (hasBit n 2, hasBit n 1, hasBit n 0), hasBit number n
    [0..7] |> Seq.map toBool
           |> dict

let toString row =
    let convert x =
        match x with
        | true -> "■"
        | _    -> " "
        
    let strs = row |> Seq.map (fun x -> async { return convert x })
                   |> Async.Parallel
                   |> Async.RunSynchronously
    String.Join("", strs)
    

let go firstRow (ruleSet:IDictionary<(bool * bool * bool), bool>) step =
    let getTriplets (row:bool[]) =
        let toTriplet index =
            match index with
            | 0                         -> (row.[0], row.[0], row.[1])
            | x when x = row.Length - 1 -> (row.[x - 1], row.[x], row.[x])
            | _                         -> (row.[index - 1], row.[index], row.[index + 1])
    
        [0..(row.Length - 1)] |> Seq.map toTriplet

    let rec goInternal row index =
        seq {
            yield row
            
            let newRow = row |> getTriplets
                             |> Seq.map (fun x -> ruleSet.Item(x))
                             |> Seq.toArray

            match index with
            | 1 -> yield newRow
            | _ -> yield! goInternal newRow (index - 1)
        }
        
    goInternal firstRow step

let rule = ruleNumber |> computeRule
let firstRow = buildBoolArray size

let results = go firstRow rule iterations
let printable = String.Join(Environment.NewLine, results |> Seq.map toString)

printable.Dump();
