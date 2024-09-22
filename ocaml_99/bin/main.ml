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


(*6. Palindrome*)

let is_palindrome = function
  | [] -> true
  | l -> l = rev l 


(*7. Flatten a List*)

type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One e :: tail -> aux (e::acc) tail
    | Many l :: tail -> aux (aux acc l) tail
  in 
  List.rev (aux [] list)




(*8. Eliminate Duplicates*)

let compress list = 
  let rec aux n list = 
    match list with
    | x :: tail -> if n = x then aux n tail
                  else n :: aux x tail
    | [] -> [n]
  in
  match list with
  | [] -> []
  | n :: tail -> aux n tail

(*another approach can be used with pattern matching first two elements of list*)


(*9. Pack Consecutive Duplicates *)

let pack list = 
  let rec aux n res list =
  match list with
    | x :: tail -> 
        if n = x then aux n (x::res) tail
        else (n :: res) :: aux x [] tail
    | [] -> [n::res]
  in
  match list with
  | [] -> []
  | n :: tail -> aux n [] tail



(*10. Run-Length Encoding*)

let encode list =
  let rec aux count n = function
  | [] -> [(count, n)]
  | x :: t -> 
    if n = x then aux (count + 1) n t
    else (count, n) :: aux 1 x t
  in
  match list with
  | [] -> []
  | x :: t -> aux 1 x t



(*11. Modified Run-Length Encoding*)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let m_encode list =
  let create_tuple count elem =
    if count = 1 then One elem
    else Many (count, elem) in 
  let rec aux count n = function
  | [] -> [create_tuple count n]
  | x :: t -> 
    if n = x then aux (count + 1) n t
    else (create_tuple count n) :: aux 1 x t
  in
  match list with
  | [] -> []
  | x :: t -> aux 1 x t













