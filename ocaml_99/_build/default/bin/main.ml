  let rec last l =
    match l with
    | [] -> None
    | [x] -> Some x
    | _ :: tail -> last tail

  let result = last ["a" ; "b" ; "c" ; "d"];;


match result with
| Some x -> Printf.printf "%s\n" x
| None -> Printf.printf  "list empty"
