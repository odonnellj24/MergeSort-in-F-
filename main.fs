#light

let ran = new System.Random(12345)

let genlist N = 
  let L = List.init N (fun i -> ran.Next(512000))
  L

//
// printfn a list: if the list is long, we print first 3 ... last 3:
//
let doprintFirst3 L = 
  printf "%A; %A; %A;" (List.item 0 L) (List.item 1 L) (List.item 2 L)
  
let doprintLast3 L = 
  let len = List.length L
  printf "%A; %A; %A" (List.item (len-3) L) (List.item (len-2) L) (List.item (len-1) L)

let printfnList msg L = 
  printf msg
  if (List.length L) <= 10 then
    printfn "%A" L
  else
    printf "["
    doprintFirst3 L
    printf " ...; "
    doprintLast3 L
    printfn "]"


//
// merge two sorted lists into one:
//
let merge L1 L2 = 
    let rec merge_ accum L1 L2 =
        match L1, L2 with
        | p, [] -> accum p
        | [], p -> accum p
        | hd1::tail1, hd2::tail2 ->
          if hd1 < hd2 then merge_ (fun x -> accum (hd1::x)) tail1 (hd2::tail2)
          else merge_ (fun x -> accum (hd2::x)) (hd1::tail1) tail2 in
    merge_ id L1 L2;;


//
// mergesort:
//
let rec mergesort L = 
  match L with
  | []    -> []
  | e::[] -> L
  | _     -> 
    let mid = List.length L / 2
    let (L1, L2) = List.splitAt mid L
    merge (mergesort L1) (mergesort L2)


//
// Input list size N:
//
let N = System.Convert.ToInt32(System.Console.ReadLine())
let L = genlist N
printfnList "L: " L
//
let R = mergesort L
printfnList "Sorted: " R
