//Statistics for an Athletic Association
//https://www.codewars.com/kata/55b3425df71c1201a800009c/train/fsharp
//"01|15|59, 1|47|6, 01|17|20, 1|32|34, 2|3|17"
//hh|mm|ss
//expected: "Range: hh|mm|ss Average: hh|mm|ss Median: hh|mm|ss"
//Remarks:
//if a result in seconds is ab.xy... it will be given truncated as ab.
//if the given string is "" you will return ""

let stat(str: string): string = 
    match str with
    | "" -> ""
    | _ -> 
        str.Split [|','|]
        |> Seq.map (fun s -> s.Trim [|' '|])
        |> Seq.map (fun s -> s.Split [|'|'|])
        |> Seq.reduce (+)