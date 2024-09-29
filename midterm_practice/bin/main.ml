(* Conceptual ideas 

Q Given the following inductive definition:

1. A slice (type 'a) is a pizza.
2. If p1 is a pizza and p2 is a pizza, then Pizza(c1,c2) is a pizza
3. Nothing else is a pizza.

a) Define a data type for pizza.

b) Define the induction principle for a given propertye P(p) where p : pizza.

Q What do we mean by first-class functions?

Q What do we mean by "partial evaluation"?

Q What do we mean by overloading? Given an example. 
  Does OCaml support overloading?

Q What do we mean by overshadowing? Give an example. 

*)

(************************************************************************)
(* Datatype : Cake                                                      *)
(************************************************************************)
(* What is a cake?

1. A slice (type 'a) is a cake of type 'a cake
2. If c1 is a cake and c2 is a cake, then Cake(c1,c2) is a cake.
3. Nothing else is a cake.

 *)

type ingredients = Cherries | Chocolate | Lemon | Oreo | Strawberries | Eggs | Flour | Blackbeans 

type 'a cake =
  | Slice of 'a
  | Cake of 'a cake * 'a cake 

let choc_cake = Cake (Slice Chocolate,
                      Cake (Slice Chocolate, Slice Chocolate))

let ccake = Cake(choc_cake, choc_cake)           

              
let mcake = Cake (Slice Cherries,
                      Cake (Slice Chocolate, Slice Strawberries))              

let mixed_cake =
  Cake (Slice [Eggs ; Flour; Chocolate] ,
        Slice [Blackbeans ; Chocolate ; Cherries])

(* Yum! Chocolate cake! *)

(* How many slices are in the cake? *)                                                       
(* count: 'a cake -> int *)
let rec count c = match c with
  | Slice _  -> 1 
  | Cake (c1, c2) -> count c1 + count c2
  
(* get all the ingredients in a cake. – What is the **most general type** of such a function? *)
(* Annotate the program with the type of the input / output below:

let rec get_all (c : ____________________ ) : _________________________ = match c with
  | Slice i -> [i]
  | Cake (c1, c2) -> (get_all c1) @ (get_all c2) 

*)


let rec get_all c = match c with
  | Slice i -> [i]
  | Cake (c1, c2) -> (get_all c1) @ (get_all c2) 


let rec get_all' c all = match c with
  | Slice i -> all @ [i]
  | Cake (c1, c2) -> (get_all' c1) (get_all' c2 all)

(* Q1 : what is the type of get_all'?

Fill in the annotations below – give the most general type:

let rec get_all' (c:___________________ ) (all: ______________________ ) : ______________________ = 
 match c with
  | Slice i -> all @ [i]
  | Cake (c1, c2) -> (get_all' c1) (get_all' c2 all)

*)
 
(* Q2: Is the above program tail-recursive?  *)


(* Q3: Does the following program below behave the same? – In other words,
   would : 

   get_all c = get_all' c []  

   be true?  
   
*)


(* ********************************************************************************************************************** *)
(* High order - croupier for simplified roulette *)

(* We have a simplified roulette, where we have only two colours that
   we can bet but if zero comes out, everyone loses *)

type colour = Red | Black        (* The two colours we can bet on *)

type result = colour option      (* The result of a run, it could be one of the colours or no colour if zero came up *)

type amt = int
type bet = amt * colour          (* The bet amount and to what colour *)

type id = string
type player = id * amt * bet option

(* It is simple to see who won *)
let compute (am, col : bet) : result -> int = function
  | None -> 0
  | Some col' -> if col = col' then am * 2 else 0

(*
Solve all these questions without using recursion or pattern
matching on lists, but instead just use the HO functions we saw
in class.
 *)

let players = [ ("Aliya", 1000, Some (400 , Red)) ;
              ("Jerome", 800, Some (240 , Black)) ;
              ("Mo" ,    900, Some (200, Black)) ;
              ("Andrea", 950, Some ((100 , Red)))]

(* Q1:  given a list of players  compute the new amounts each player has and set their bets to None *)

(* Q2: given a list of bets and a result compute a list of winning players with their bets *)

(* Q3: given a list of bets and a result compute how much money the casino needs to pay back *)


(* Q4: given a list of bets and a result compute if nobody won *)

(* Q4: everybody won !*)

(* Q5: given a list of bets and a result compute if someone won *)

(* Q6: given a list of bets return the highest winning *)

(* Level-up (a bit more complicated) *)

(* Q7: given a list of bets and a result compute the balance for the casino, how much it made *)

(* Ninja level  *)

(* Q8: Can you sort the results by the amount they made? *)





