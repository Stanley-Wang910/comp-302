
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


