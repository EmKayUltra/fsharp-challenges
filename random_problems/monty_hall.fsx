// let counts = (0,0)

// let inline rand n = System.Random().Next(n)

// let options = [ for x in 0..9999 -> Set.ofList [0;1;2] ]
// let initial_choices = [ for x in 0..9999 -> rand 3 ] //seq { for x in 1 .. 9999 -> rand 3 }
// let winners =  [ for x in 0..9999 -> rand 3 ]
// let losers = options |> List.mapi (fun i x -> x.Remove(winners.[i]))
// let swap_choices = options |> List.mapi (fun i x -> x.Remove(initial_choices.[i]))
//   |> List.map (fun x -> x.Remove())

// initial_choices
//   |> List.zip winners
//   |> List.filter (fun (c,w) -> c = w)
//   |> List.length
//   |> fun x -> "keep win rate: "+(float x/10000.0).ToString()

// swap_choices
//   |> List.zip winners
//   |> List.filter (fun (c,w) -> c = w)
//   |> List.length
//   |> fun x -> "swap win rate: "+(float x/10000.0).ToString()



let monty_hall () =
  let random = System.Random()
  let inline rand n = random.Next(n)

  let attempt_swap choice prize =
    let element_to_remove = 
      [0..2] 
        |> List.filter (fun x -> x <> choice && x <> prize) //these CAN be removed
        |> fun x -> List.item (rand x.Length) x  //remove one at random

    let new_choice = [0..2] |> List.find (fun x -> x <> element_to_remove && x <> choice) //filter out the element to be removed, and current choice

    if (new_choice = prize) then 1 else 0

  let attempt_keep choice prize = 
    if (prize = choice) then 1 else 0

  let choices_and_prizes = [ for x in 0..9999 -> (rand 3, rand 3) ]

  choices_and_prizes 
    |> List.fold (fun (keepwins,swapwins) (choice,prize) -> (keepwins+(attempt_keep choice prize),swapwins+(attempt_swap choice prize))) (0,0)
    |> fun (keepwins,swapwins) ->  "keep win rate: "+((float keepwins)/10000.0).ToString()+" ("+keepwins.ToString()+"); swap win rate: "+((float swapwins)/10000.0).ToString()+" ("+swapwins.ToString()+")"
 

monty_hall()