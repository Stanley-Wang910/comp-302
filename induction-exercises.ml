(* Additional Exercises / Prep for the midterm 
   with a particular focus on

   - working with data structures such as lists/trees

   - reasoning about programs by induction
     (in particular structural induction on lists and trees)

   - understand what we mean by tail-recursion
     and how to transform a program into a tail-recursive one,
     if possible

   Autor: Brigitte Pientka
*)

exception Fill_Blank 

(* ************************************************************************************* *)
(* QUESTION *)

(* Here is a program that computes the length of a list *)

let rec length (l:'a list) : int = match l with 
  | [] -> 0 
  | _::xs -> 1 + length xs

(*

1) Implement a tail-recursive version length_tr of the above size function that uses 
   an accumulator. 
   What would be the type of such a function?

A: The type of the function would be 'a list -> int -> int 


2) What initial value init do you need to pass to length_tr? 

A: 0 would be the initial value

3) Prove that length l computes the same result as calling length_tr t init (i.e. where we give the initial value as the second argument)
   (HINT: This might require a generalization of the theorem; see how we proved length l = length_tr l init) 

  Prove that for all lists l and integers acc, length_tr l acc = length l + acc


  Base Case: l = []
  Show: length_tr [] acc = length [] + acc = 0 + acc

    = evaluation (pattern matching)
    | [] -> acc 
    acc = 0 + acc

  Inductive Hypothesis:
    for a list t and any acc, it holds that
    length_tr t acc = length t + acc

  Inductive Step: l = (h::t)
  Show: length_tr l acc = length l + acc = 1 + length t + acc

     = eval (pattern matching)
     | h::t -> length_tr t (acc + 1)
     by IH, length_tr t (acc +1) = length t + (acc + 1) = 1 + length t + acc 
     
     = length (h::t = l) + acc

*)

let rec length_tr (l:'a list) (acc:int) : int = match l with
  | [] -> acc
  | _::tail -> length_tr tail (acc + 1)

(* ************************************************************************************* *)
(* QUESTION *)
(* In class we wrote two versions of summing up the elements up to n *)

let rec sum n =
  if n = 0 then
    0
  else n + sum(n-1)

let rec sum_tr n acc =
    if n = 0 then
      acc
    else
      sum_tr (n-1)  (acc + n)

(* Prove that sum n evaluates to the same result as sum_tr n 0 *)

(* ************************************************************************************* *)
(* QUESTION *)
(* Recursively add up all the elements in a list. 

   For example: sum_up  [1,2,3] = 6
*)
let rec sum_up (l:int list) : int = match l with 
| [] -> 0
| x::xs -> x + sum_up xs

(*

1) Implement a tail-recursive version sum_up_tr of the above size function that uses 
   an accumulator. 

   What would be the type of such a function?

2) What initial value init do you need to pass to sum_up_tr? 

3) Prove that sum_up  l computes the same result as calling sum_up_tr t init (i.e. where we give the initial value as the second argument)
   (HINT: This might require a generalization of the theorem; see how we proved sum_up l  = sum_up_tr l init) 

*)

(* ************************************************************************************* *)
(* QUESTION *)
 type 'a tree = Empty | Node of 'a * 'a tree * 'a tree


(* size of a tree
   size: 'a tree -> int

   size(T) = n where n is size of the tree determined by
   the number of nodes. 
*)
let rec size (t:'a tree) : int = match t with 
| Empty          -> 0
| Node (a, l, r) -> size l + size r + 1


let rec size_tr (t: 'a tree) (acc: int) = match t with 
| Empty -> acc
| Node (v, l, r) -> let l_tree = size_tr l acc
                    in size_tr r (1 + l_tree)


(* PRACTICE QUESTION – OFFLINE – 

   Here is what I would expect you to do by the end of this week:

1) Implement a version size_tr of the above size function that uses 
   an accumulator.  size_tr (t: 'a tree) (acc:int) : int

   Would this function be tail-recursive? – Recall what tail-recursion means.
   It is an optimization that eliminates the overhead of generating stack frames 
   when recursively calling a function.

2) What initial value init do you need to pass to size_tr? 

3) Prove that size t computes the same result as calling size_tr t init (i.e. where we give the initial value as the second argument)
   (HINT: This might require a generalization of the theorem; see how we proved rev l = rev_tr l []) 
*)

(* collect all elements in a tree – right-to-left traversal *)

let rec collect (t: 'a tree) : 'a list = raise Fill_Blank 

(* Prove : size (t) = length (collect t) *)


