//"5035" => "5000 + 30 + 5"
let expanded_Form(num: int64) =
    num.ToString() 
    |> Seq.toList 
    |> List.rev
    |> List.mapi (fun i s -> s.ToString() + String.replicate i "0")
    |> List.filter (fun s -> s |> int64 <> 0L)  
    |> List.rev
    |> String.concat " + "


// REFACTORED
//"5035" => "5000 + 30 + 5"
let expandedForm(num: int64) =
    num.ToString() 
    |> Seq.toList 
    |> List.rev
    |> List.indexed
    |> List.fold (fun acc (i,s) -> if s = '0' then acc else [s.ToString() + (String.replicate i "0")] @ acc) []
    |> String.concat " + "

let printExpected = printfn "FAIL, EXPECTED: %s, ACTUAL: %s"
let printSuccess = printfn "SUCCESS: %s = %s"
let fakeAssert a b =
        if (a = b) then printSuccess a b else printExpected a b
fakeAssert "5000 + 30 + 5" (expandedForm (int64 5035))