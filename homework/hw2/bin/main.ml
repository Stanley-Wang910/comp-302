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




(* Q2 *)

(*record composite of other data types*)
type exp = 
  | PLUS  of exp * exp 
  | MINUS of exp * exp
  | MULT  of exp * exp 
  | DIV   of exp * exp
  | SIN   of exp     
  | COS   of exp     
  | EXP   of exp 
  | FLOAT of float  


(*(MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0))*)
(*wait thats actually mad powerful*)

(* TODO: Write a good set of tests for eval *)
let eval_tests = [
  (FLOAT 0.0, 0.0);
  (FLOAT (-42.0), -42.0);
  (FLOAT 3.14159, 3.14159);

  (PLUS (FLOAT 2.0, FLOAT 3.0), 5.0);
  (PLUS (FLOAT (-1.0), FLOAT 1.0), 0.0);

  (MINUS (FLOAT 5.0, FLOAT 2.0), 3.0);
  (MINUS (FLOAT 0.0, FLOAT 5.0), -5.0);

  (MULT (FLOAT 3.0, FLOAT 4.0), 12.0);
  (MULT (FLOAT (-2.0), FLOAT 3.0), -6.0);

  (DIV (FLOAT 10.0, FLOAT 2.0), 5.0);
  (DIV (FLOAT 5.0, FLOAT (-1.0)), -5.0);

  (SIN (FLOAT 0.0), 0.0);
  (SIN (FLOAT (3.1415926535 /. 2.0)), 1.0);  (* Approximately 1 *)

  (COS (FLOAT 0.0), 1.0);
  (COS (FLOAT 3.1415926535), -1.0);  (* Approximately -1 *)

  (EXP (FLOAT 0.0), 1.0);
  (EXP (FLOAT 1.0), exp 1.0);

  (PLUS (SIN (FLOAT 0.0), COS (FLOAT 0.0)), 1.0);
  (MULT (PLUS (FLOAT 2.0, FLOAT 3.0), FLOAT 4.0), 20.0);
  (DIV (MULT (FLOAT 9.0, FLOAT 2.0), PLUS (FLOAT 3.0, FLOAT 3.0)), 3.0);

  (EXP (PLUS (FLOAT 1.0, MULT (FLOAT 2.0, FLOAT 3.0))), exp 7.0);
  (SIN (EXP (FLOAT 0.0)), sin 1.0);

]

(* TODO: Implement eval. *)
let rec eval e = 
  match e with
  | PLUS  (e1, e2) -> (eval e1) +. (eval e2)
  | MINUS (e1, e2) -> (eval e1) -. (eval e2)
  | MULT  (e1, e2) -> (eval e1) *. (eval e2)
  | DIV   (e1, e2) -> (eval e1) /. (eval e2)
  | SIN   e ->  sin (eval e)
  | COS   e ->  cos (eval e)
  | EXP   e ->  exp (eval e)
  | FLOAT f -> f


type instruction = 
  Plus | Minus | Mult | Div | Sin | Cos | Exp | Float of float	


(*(2.2 + 3.3) * 5.0 == [Float 2.2; Float 3.3; Plus; Float 5.0; Mult]	*)
(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  (FLOAT 2.2, [Float 2.2]);

  (PLUS (FLOAT 2.2, FLOAT 3.3), [Float 2.2; Float 3.3; Plus]);

  (MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0),
   [Float 2.2; Float 3.3; Plus; Float 5.0; Mult]);

  (PLUS (PLUS (FLOAT 2.2, FLOAT 3.3), MULT (FLOAT 5.5, FLOAT 6.6)),
   [Float 2.2; Float 3.3; Plus; Float 5.5; Float 6.6; Mult; Plus]);

  (SIN (FLOAT 0.0), [Float 0.0; Sin]);

  (COS (PLUS (FLOAT 0.0, FLOAT 1.0)), [Float 0.0; Float 1.0; Plus; Cos]);

  (DIV (FLOAT 10.0, MINUS (FLOAT 5.0, FLOAT 3.0)),
   [Float 10.0; Float 5.0; Float 3.0; Minus; Div]);
  (EXP (MULT (SIN (FLOAT 0.5), COS (FLOAT 0.5))),
   [Float 0.5; Sin; Float 0.5; Cos; Mult; Exp]);

  (MINUS (EXP (FLOAT 1.0), DIV (FLOAT 10.0, MULT (FLOAT 2.0, FLOAT 5.0))),
   [Float 1.0; Exp; Float 10.0; Float 2.0; Float 5.0; Mult; Div; Minus]);

  (SIN (COS (EXP (FLOAT 1.0))),
   [Float 1.0; Exp; Cos; Sin]);
]

(* TODO: Implement to_instr. *)
(*use @ to concatenate lists*)
let rec to_instr e = 
  match e with
  | PLUS  (e1, e2) -> to_instr e1 @ to_instr e2 @ [Plus]
  | MINUS (e1, e2) ->  to_instr e1 @ to_instr e2 @ [Minus]
  | MULT  (e1, e2) -> to_instr e1 @ to_instr e2 @ [Mult]
  | DIV   (e1, e2) -> to_instr e1 @ to_instr e2 @ [Div]
  | SIN   e ->   to_instr e @ [Sin]
  | COS   e ->  to_instr e @ [Cos]
  | EXP   e -> to_instr e @ [Exp]
  | FLOAT f -> [Float f]



(* TODO: Write a good set of tests for instr *)



type stack = float list

(* TODO: Implement to_instr. *)               
let instr i s =
  match i with
  | Float f -> Some (f :: s)
  | Plus ->
      (match s with
       | a :: b :: rest -> Some ((b +. a) :: rest)
       | _ -> None)
  | Minus ->
      (match s with
       | a :: b :: rest -> Some ((b -. a) :: rest)
       | _ -> None)
  | Mult ->
      (match s with
       | a :: b :: rest -> Some ((b *. a) :: rest)
       | _ -> None)
  | Div ->
      (match s with
       | 0.0 :: _ -> None
       | a :: b :: rest -> Some ((b /. a) :: rest) 
       | _ -> None)
  | Sin ->
      (match s with
       | a :: rest -> Some ((sin a) :: rest)
       | _ -> None)
  | Cos ->
      (match s with
       | a :: rest -> Some ((cos a) :: rest)
       | _ -> None)
  | Exp ->
      (match s with
       | a :: rest -> Some ((exp a) :: rest)
       | _ -> None)
(* TODO: Write a good set of tests for prog *)
let prog_tests = [
]

(* TODO: Implement prog. *)
let prog_tests = [
  ([Float 3.0; Float 2.0; Plus], (Some 5.0));
  
  ([Float 10.0; Float 2.0; Div], (Some 5.0));
  ([Float 2.0; Float 3.0; Float 4.0; Mult; Plus], (Some 14.0));
  ([Float 1.0; Sin; Cos], (Some 0.66636674539));
  ([Float (-1.0); Exp], (Some 0.3678794412));
]

(* TODO: Implement prog. *)
let prog instrs = 
  let rec aux instrs stack =
    match instrs with
    | [] -> 
        (match stack with
         | [result] -> Some result  (* Return the single result *)
         | _ -> None)  (* Return None if stack doesn't have exactly one element *)
    | Float f :: rest -> aux rest (f :: stack)
    | Plus :: r_instrs ->
        (
          match stack with 
          | a :: b :: r_stack -> aux r_instrs ((b +. a) :: r_stack)
          | _ -> None
        )
    | Minus :: r_instrs ->
        (
          match stack with 
          | a :: b :: r_stack -> aux r_instrs ((b -. a) :: r_stack)
          | _ -> None
        )
    | Mult  :: r_instrs ->
        (
          match stack with 
          |a :: b :: r_stack -> aux r_instrs ((b *. a) :: r_stack)
          | _ -> None
        )
    | Div :: r_instrs ->
        (
          match stack with 
          | a :: b :: r_stack -> aux r_instrs ((b /. a) :: r_stack)
          | _ -> None
        )
    | Exp :: r_instrs ->
        (
          match stack with 
          | a :: r_stack -> aux r_instrs ((exp a) :: r_stack)
          | _ -> None
        )
    | Sin :: r_instrs ->
        (
          match stack with 
          | a :: r_stack -> aux r_instrs ((sin a) :: r_stack)
          | _ -> None
        )
    | Cos :: r_instrs ->
        (
          match stack with 
          | a :: r_stack -> aux r_instrs ((cos a) :: r_stack)
          | _ -> None
        )

  in 
  aux instrs []
