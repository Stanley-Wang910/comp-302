exception NotImplemented
exception Error

(* Q1 The type of nucleobase. *)
type nucleobase = T | A | C | G



(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
  ([], []);
  
  ([A], [(1, A)]);
  
  ([C; C; C; C], [(4, C)]);
  
  ([A; G; C; T], [(1, A); (1, G); (1, C); (1, T)]);
  
  ([A; G; A; G; A; G], [(1, A); (1, G); (1, A); (1, G); (1, A); (1, G)]);
  
  ([T; T; A; A; A; G; T; T; C; C; C; G; G; A; A; T], 
   [(2, T); (3, A); (1, G); (2, T); (3, C); (2, G); (2, A); (1, T)]);
  ([A; A; G; C; C; T; T; T; G; A; A; A; C; T], 
   [(2, A); (1, G); (2, C); (3, T); (1, G); (3, A); (1, C); (1, T)]);
  
  ([G; G; G; G; G], [(5, G)]);
  
  ([A; A; G; G; C; C; T; T], [(2, A); (2, G); (2, C); (2, T)]);
  
  ([C; G; G; A; T; C; C; T; A; A; G; T; C; G; G; G], 
   [(1, C); (2, G); (1, A); (1, T); (2, C); (1, T); (2, A); (1, G); (1, T); (1, C); (3, G)]);
  
  ([A; A; A; A; G; G; A; T; T; T; C; T; C], 
   [(4, A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)]);
]
(* TODO: Implement compress. *)
(*iterate through input list of nucleobases*)
(*keep track of cur, and its count*)
(*when different nucleobase, add count and prev to res*)
(*iterate until end of list*)
(*add last group after iterating*)
(*maybe build res in reverse, so appending won't require iterating?*)
let compress (l : nucleobase list) : (int * nucleobase) list =
  let rec aux count prev result = function
    | [] -> List.rev ((count, prev) :: result)
    | cur :: rest ->
      if cur = prev then
        aux (count + 1) cur result rest
      else aux 1 cur ((count, prev) :: result) rest
  in 
  match l with
  | [] -> []
  | head :: tail ->
    aux 1 head [] tail

(* TODO: Write a good set of tests for decompress *)
(* A good set of tests for decompress *)
let decompress_tests = [
  ([], []);
  
  ([(1, A)], [A]);
  
  ([(4, C)], [C; C; C; C]);
  
  ([(1, A); (1, G); (1, C); (1, T)], [A; G; C; T]);
  
  ([(1, A); (1, G); (1, A); (1, G); (1, A); (1, G)], [A; G; A; G; A; G]);
  
  ([(2, T); (3, A); (1, G); (2, T); (3, C); (2, G); (2, A); (1, T)], 
   [T; T; A; A; A; G; T; T; C; C; C; G; G; A; A; T]);
  
  ([(2, A); (1, G); (2, C); (3, T); (1, G); (3, A); (1, C); (1, T)], 
   [A; A; G; C; C; T; T; T; G; A; A; A; C; T]);
  
  ([(5, G)], [G; G; G; G; G]);
  ([(2, A); (2, G); (2, C); (2, T)], [A; A; G; G; C; C; T; T]);
  
  ([(1, C); (2, G); (1, A); (1, T); (2, C); (1, T); (2, A); (1, G); (1, T); (1, C); (3, G)], 
   [C; G; G; A; T; C; C; T; A; A; G; T; C; G; G; G]);
  
  ([(4, A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)], 
   [A; A; A; A; G; G; A; T; T; T; C; T; C]);
]

(* TODO: Implement decompress. *)
let decompress (l : (int * nucleobase) list) : nucleobase list =
  let rec aux res = function
    | [] -> List.rev(res)
    | (count, base) :: rest ->
        let rec prepend n acc=
          if n <= 0 then acc
          else prepend (n-1) (base :: acc)
        in 
        aux (prepend count res) rest
  in
  aux [] l



