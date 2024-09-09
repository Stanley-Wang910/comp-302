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
  (5, 10.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float =
  match n with
  | 0 -> 1.
  | _ -> n * factorial n - 1


(* TODO: Write your own tests for the binomial function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 0.);
  ((1, 0), 0.);
  ((2, 0), 1.);
  ((10, 1), 1.);
  ((10, 2), 1.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial (n: int) (k: int) =
  if n < 0
  then domain ()
  else (if k = n
        then domain ()
        else fact k /. (fact n *. fact (k - n)))


(* TODO: Write a good set of tests for ackerman. *)
let ackerman_tests = [
  (* Your test cases go here *)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let ackerman (n, k)  =
  if n < 0 || n < 0
  then domain ()
  else (let rec ack n k =
          match (n, k) with
          | (_, 0) -> k + 1
          | (0, _) -> ack n 1 -1
          | (_, _) -> ack (n - 1) (ack n (k - 1))
        in ack n k)
