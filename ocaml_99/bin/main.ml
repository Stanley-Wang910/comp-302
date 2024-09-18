(*1. Tail of a List*)
let rec last l =
  match l with
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> last tail

let result = last ["a" ; "b" ; "c" ; "d"];;


 
(*the official solution *)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> last tail

let result = last ["a" ; "b" ; "c" ; "d"];;



(*2. Last Two Elements of a List*)
let rec last_two list =
  match list with
  | [] -> None
  | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: tail -> last_two tail

(*3. Nth Element of a List*)

let rec nth_elem k list =
  match list with
  | [] -> None
  | x :: tail -> if k > 0 then nth_elem (k-1) tail else Some x 


(*4. Length of a List*)
 (*-- can do tail recursion too *)

let length list =
  let rec aux k = function
    | [] -> k
    | _ :: tail -> aux (k+1) tail
  in
  aux 0 list


(*5. Reverse a List*)

let rev list =
  let rec aux r_l = function
    | [] -> r_l
    | x :: tail -> aux (x::r_l) tail
  in 
  aux [] list


