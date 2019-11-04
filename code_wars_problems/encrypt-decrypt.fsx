
//Simple Encryption #1 - Alternating Split
let rec encrypt (text:string) (n:int) = 
    match text, n with
    | text, n when n <= 0 -> text
    | "", _ -> ""
    | null, _ -> null
    | text, _ -> 
        text 
        |> Seq.toList 
        |> List.indexed 
        |> List.fold (fun (a,b) (i,x) -> if i % 2 = 0 then (a, b @ [x]) else (a @ [x], b)) ([],[]) 
        |> fun (e,o) -> e @ o //convert tuple of lists to single list
        |> List.map string //convert chars to strings
        |> List.reduce (+) //concat the letters
        |> fun x -> encrypt x (n-1); 
//    str |> Seq.toList |> List.indexed |> List.fold (fun acc (i,x) -> if i % 2 <> 0 then [acc.Item(0); acc.Item(1) @ [x]] else [acc.Item(0) @ [x]; acc.Item(1)]) [[];[]] |> List.concat //this works!
    // str |> Seq.toList |> List.indexed |> List.partition (fun (i,x) -> i % 2 = 0) //partition is returning a weird type, for "asdf" i'm getting ([(0, 'a'); (2, 'd')], [(1, 's'); (3, 'f')])

    // if n = 0 then str
    // else
        // let indexed =
        //     str
        //     |> Seq.toList
        //     |> List.indexed

        // let first = 
        //     indexed
        //     |> List.filter (fun (i,x) -> i % 2 <> 0)
        //     |> List.map (fun (i,x) -> x.ToString())
        //     |> String.concat ""

        // let second = 
        //     indexed
        //     |> List.filter (fun (i,x) -> i % 2 = 0)
        //     |> List.map (fun (i,x) -> x.ToString())
        //     |> String.concat ""

        // encrypt (first+second) (n-1)
    

let rec decrypt (encryptedText:string) (n:int) = 
    match encryptedText, n with
    | encryptedText, n when n <= 0 -> encryptedText
    | "", _ -> ""
    | null, _ -> null
    | encryptedText, _ -> 
        (encryptedText.[0..(encryptedText.Length/2-1)], encryptedText.[(encryptedText.Length/2)..]) //split in half
        |> fun (e,o) -> Seq.zip o e
        |> Seq.toList
        |> List.map (fun (x,y) -> x.ToString() + y.ToString())
        |> List.reduce (+)
        |> fun x -> (if encryptedText.Length % 2 = 0 then x else x + encryptedText.[encryptedText.Length-1].ToString()) //seq.zip will allow zipping of uneven sets, but we need to append the truncated character
        |> fun x -> decrypt x (n-1)
//(if encryptedText.Length % 2 = 0 then encryptedText.[0..(encryptedText.Length/2-1)] else encryptedText.[0..(encryptedText.Length/2-1)] @ [""])
//(encryptedText.[0..(encryptedText.Length/2-1)] |> Seq.toList, encryptedText.[(encryptedText.Length/2)..] |> Seq.toList) //split in half
// |> fun (e,o) -> List.zip e o

let printExpected = printfn "FAIL, EXPECTED: %s, ACTUAL: %s"
let printSuccess = printfn "SUCCESS: %s = %s"
let fakeAssert a b =
        if (a = b) then printSuccess a b else printExpected a b
fakeAssert "ia esrigh Tst" (encrypt "This is great" 5) 
fakeAssert "This is great" (decrypt "ia esrigh Tst" 5)
fakeAssert "This is great" (decrypt (encrypt "This is great" 5) 5)