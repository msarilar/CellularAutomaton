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

let size = 200
let ruleNumber = 30
let iterations = size / 2

let getElements (row:bool[]) =
    let toTriplet index =
        match index with
        | 0                         -> (false, row.[0], row.[1])
        | x when x = row.Length - 1 -> (row.[x - 1], row.[x], false)
        | _                         -> (row.[index - 1], row.[index], row.[index + 1])

    [0..(row.Length - 1)] |> Seq.map toTriplet
    
let computeRule number =
    let hasBit num bit =
        num &&& (1 <<< bit) > 0
    let toBool n =
        (hasBit n 2, hasBit n 1, hasBit n 0), hasBit number n
    [0..7] |> Seq.map toBool
           |> dict

let toString (row:bool[]) =
    let convert x =
        match x with
        | true -> "■"
        | _    -> " "
        
    let strs = row |> Seq.map (fun x -> async { return convert x })
                   |> Async.Parallel
                   |> Async.RunSynchronously
    String.Join("", strs)
    

let go (row:bool[]) (ruleSet:IDictionary<(bool * bool * bool), bool>) step =
    let interpret triplet =
        ruleSet.Item(triplet)

    let rec goInternal (row:bool[]) index =
        seq {
            yield toString row
            
            let newRow = row |> getElements
                             |> Seq.map (fun x -> async { return interpret x })
                             |> Async.Parallel
                             |> Async.RunSynchronously
                             |> Seq.toArray

            match index with
            | 1 -> yield toString newRow
            | _ -> yield! goInternal newRow (index - 1)
        }
        
    goInternal row step

let build n =
    [0..n] |> Seq.map (fun x -> match x with
                                | x when x = n / 2 -> true
                                | _ -> false)
           |> Seq.toArray


let rule = computeRule ruleNumber
let arr = build size

go arr rule iterations |> Seq.map (fun x -> x.Dump())
                       |> Seq.toList
                       |> ignore