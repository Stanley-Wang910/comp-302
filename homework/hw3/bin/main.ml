(*prelude*)

exception NotImplemented

let fact n =
  let rec factorial n =
    if n = 0 then 1
    else  n * factorial (n - 1)
  in
  if n <= 0 then 1 else factorial n

let binom (n, k) =
  if n < k then 0.0
  else float (fact n) /. (float (fact k) *. float (fact (n - k)))

let dist_black n x (marblesTotal, marblesDrawn) =
  (binom (n, x) *. binom (marblesTotal - n, marblesDrawn - x))
  /. (binom (marblesTotal, marblesDrawn))

let tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n-1) ((f n)::acc)
  in
  tab n []

let max_in_list l =
  let rec max_in_list' pos l =
    match l with
    | [] -> assert false
    | [h]  -> (pos, h)
    | h::t ->
      let (q, mx) = max_in_list' (pos + 1) t in
      if h < mx then (q, mx)
      else (pos, h)
  in
  let (pos, _) = max_in_list' 0 l in
  pos


 type ingredients = Chocolate | Orange | Almonds | Vanilla | Flour | BlackBeans

 type cake = Slice of ingredients list | Cake of cake * cake

                                            
let rec insert x l = match l with
  | [] -> [x]
  | y::ys -> if x = y then l else y::(insert x ys)
           
let rec union l1 l2 = match l2 with
  | [] -> l1
  | x::xs -> union (insert x l1) xs



(*Problem 1*)

(* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (((fun x -> x), -1), []);
  (((fun x -> x), 3), [0; 1; 2; 3]);  
  (((fun _ -> 5), 4), [5; 5; 5; 5; 5]);
  (((fun x -> x), 0), [0]);  
  (((fun x -> x + 1), 2), [1; 2; 3]);
  (((fun x -> 2 * x), 5), [0; 2; 4; 6; 8; 10]);
  (((fun x -> x mod 3), 10), [0; 1; 2; 0; 1; 2; 0; 1; 2; 0; 1]);
]


(* TODO: Implement dist_table: (int * int) -> int -> float list *)
let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list =
  let fix_n n =
    dist_black n x (marblesTotal, marblesDrawn) 
  in
  tabulate fix_n marblesTotal


let is_empty_tests: (float list list * bool) list = [
  ([], true);
  ([[]], true);
  ([[]; []; []], true);
  ([[0.]], false);
  ([[0.]; []], false);
  ([[]; [0.]], false);
  ([[0.]; [1.]; [2.]], false);
  ([[]; []; [0.1; 0.2]], false)
]
(* TODO: Implement is_empty: 'a list list -> bool *)
let is_empty (matrix: 'a list list) : bool =
  if List.for_all (fun l -> List.length l = 0) matrix then true
    else false

(* TODO: Implement dist_matrix: int * int -> int list -> float list list *)
let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list =
  let aux x =
    dist_table (total, drawn) x
  in 
  List.map aux resultList


let rec combined_dist_table (matrix: float list list) =
  let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | lists -> (
      List.map List.hd lists :: transpose (List.map List.tl lists))
  in 
  List.map (List.fold_left ( *. ) 1.) (transpose matrix)
