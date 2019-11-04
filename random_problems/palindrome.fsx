let inline rev_string (str:string) = str |> List.ofSeq |> List.rev |> List.map string |> List.reduce (+)
let inline is_palindrome (str:string) = str.Length > 1 && (str = rev_string str)

let rec palindrometer (str:string) =
    match str, str.Length with
    | _, n when n < 2 -> []
    | str, _ ->
        let my_palindromes = 
            [0..str.Length-1]
            |> List.map (fun i -> str.[0..i]) //generate every string possible from this index forward
            |> List.filter is_palindrome 

        [1..str.Length-1] //now recursively do this for every substring starting at every subsequent char
        |> List.fold (fun palindromes i -> palindromes @ (palindrometer str.[i..])) my_palindromes
        |> List.distinct


// TESTING
let printExpected = printfn "FAIL, EXPECTED: %s, ACTUAL: %s"
let printSuccess = printfn "SUCCESS: %s = %s"
let fakeAssert (a:List<string>) (b:List<string>) =
        if ((a |> List.sort) = (b |> List.sort)) then printSuccess (a.ToString()) (b.ToString()) else printExpected (a.ToString()) (b.ToString())

fakeAssert ["abcba"; "bcb"] (palindrometer "abcba" )
fakeAssert [] (palindrometer "cat" )
fakeAssert [] (palindrometer "a" )
fakeAssert ["aa"] (palindrometer "aa" )
fakeAssert ["baaab"; "aaa"; "aa"] (palindrometer "baaab" )
fakeAssert ["pop"; "dad"; "ada"] (palindrometer "apopcdada" )
fakeAssert ["dadxdad"; "dad"; "adxda"; "dxd"] (palindrometer "dadxdad" )