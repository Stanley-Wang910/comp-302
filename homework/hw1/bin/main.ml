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




(* Question 2: is_prime *)

(* TODO: Write a good set of tests for is_prime. *)
let is_prime_tests = [
(* Your tests go here *)
  (2, true);
  (3, true);

  (4, false);
  (5, true);

  (6, false);
  (7, true);
  (1523, true);
  (7727, true);
  (7728, false);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let is_prime (n: int) : bool =
  if n <= 1 then domain () 
  else
    let rec divisor x =
      if x * x > n
        then true
      else if n mod x == 0 
        then false
      else
        divisor (x+1)
      in 
        divisor 2

(* Question 3: Newton-Raphson method for computing the square root *)

let square_root_tests = [
  (1e-300, 1e-150);
  (2.0, 1.4142135623730951);
  (3.0, 1.7320508075688772);
  (5.0, 2.236067977499790);
  (4., 2.);
  (9.,3.);
  (16., 4.);
  (25., 5.);
  (36., 6.);
  (49., 7.);
  (64., 8.);
  (81., 9.);
  (100., 10.);
  (1., 1.);
]

let square_root (a : float) =
  let rec findroot x acc =
    let x_prime = ((a /. x) +. x) /. 2. in 
  if abs_float (x -. x_prime) < acc then x_prime
  else
    findroot x_prime acc
  in
  if a > 0.
  then findroot 1. epsilon_float
  else domain ()



(* Question 4: Fibonacci*)

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  (0, 0);
  (1, 1);
  (2, 1);
  (3, 2);
  (4, 3);
  (5, 5);
  (6, 8);
  (7, 13);
  (8, 21);
  (9, 34);
  (10, 55);
  (15, 610);
  (20, 6765);
  (30, 832040);
  (40, 102334155);
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b =
  if n = 0 then a
  else
  fib_aux (n-1) (b) (a+b)
  


(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n =
  match n with
  | 0 -> 1
  | 1 -> 1
  | _ -> fib_aux n 1 1 
