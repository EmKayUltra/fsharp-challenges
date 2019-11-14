(*
Your task is to add up letters to one letter.

The function will be given a variable amount of arguments, each one being a letter to add.

Notes:
Letters will always be lowercase.
Letters can overflow (see second to last example of the description)
If no letters are given, the function should return 'z'
Examples:
add_letters('a', 'b', 'c') = 'f'
add_letters('a', 'b') = 'c'
add_letters('z') = 'z'
add_letters('z', 'a') = 'a'
add_letters('y', 'c', 'b') = 'd' # notice the letters overflowing
add_letters() = 'z'
*)

let add_letters (args:List<string>) = 
    match args with
    | [x] -> x
    | [] | [""] -> "z"
    | _::_ -> 
        args 
        |> List.map char 
        |> List.map (fun x -> int x - int 'a' + 1) // convert to 1-based index starting at 'a'
        |> List.fold (fun acc c -> ((c + acc) % (int 'z'-int 'a' + 1))) 0 //sum up the numbers one by one, if the sum exceeds the difference between a and z, reset to 0
        |> fun x -> char (int 'a' - 1 + x) //add the difference to 'a'
        |> string
        
// TESTING
let printExpected = printfn "FAIL, EXPECTED: %s, ACTUAL: %s"
let printSuccess = printfn "SUCCESS: %s = %s"
let fakeAssert a b =
        if a = b then printSuccess a b else printExpected a b

fakeAssert "z" (add_letters [] )
fakeAssert "a" (add_letters ["a"] )
fakeAssert "f" (add_letters ["a";"b";"c"])
fakeAssert "c" (add_letters ["a";"b"])
fakeAssert "a" (add_letters ["z";"a"])
fakeAssert "d" (add_letters ["y";"c";"b"])