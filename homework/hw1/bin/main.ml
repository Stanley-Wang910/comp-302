(* Question 1 *)
(* TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
(* TODO: Correct these tests for the fact function. *)
let fact_tests = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (3, 6.);
  (4, 24.);
  (5, 120.);
  (6, 720.);
  (7, 5040.);
  (8, 40320.);
  (10, 3628800.);
  (12, 479001600.);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float =
  match n with
  | 0 -> 1.
  | _ -> float_of_int n *. fact (n - 1)


let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((38, 27), 1203322288.0);
  ((40, 12), 5586853480.0);
  ((29, 15), 77558760.0);
  ((33, 33), 1.0);
  ((19, 7), 50388.0);
  ((36, 22), 3796297200.0);
  ((25, 18), 480700.0);
  ((31, 9), 20160075.0);
  ((22, 3), 1540.0);
  ((37, 26), 854992152.0);
  ((14, 11), 364.0);
  ((39, 35), 82251.0);
  ((28, 20), 3108105.0);
  ((17, 5), 6188.0);
  ((34, 16), 2203961430.0);
  ((0, 0), 1.0);
  ((1, 0), 1.0)
]



(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial (n: int) (k: int) : float =
  if n < 0 || k < 0 || k > n
    then domain ()
  else if k = n || k = 0
    then 1.
  else 
    fact n /. (fact k *. fact (n - k))

(* TODO: Write a good set of tests for ackerman. *)
let ackerman_tests = [
  (* Basic cases *)
  ((0, 0), 1);
  ((0, 1), 2);
  ((0, 2), 3);
  ((1, 0), 2);
  ((1, 1), 3);
  ((2, 0), 3);
  ((2, 1), 5);
  ((3, 0), 5);
  
  (* Edge cases *)
  ((0, 5), 6);  (* m = 0 *)
  ((1, 5), 7);  (* simple recursive case *)
  ((2, 2), 7);  (* double recursive case *)
  
  (* Larger values (be cautious with these, as they grow very quickly) *)
  ((3, 1), 13);
  ((3, 2), 29);
  
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let ackerman (n, k)  =
  if n < 0 || k < 0
    then domain ()
  else 
    (let rec ack n k =
       match (n, k) with
       | (0, 0) -> 1 
       | (_, 0) -> ack (n-1) 1
       | (0, _) -> k + 1
       | (_, _) -> ack (n - 1) (ack n (k - 1))
     in ack n k)
