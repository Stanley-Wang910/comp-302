(*let m = 4 in*)
(*let n = m * m in*)
(*let k = m * m in*)
(*  k*n*)
(*;;*)

(*function example*)


let pi = 3.14
(*  name   input    output *)
let area : float -> float = 
  function r -> pi *. r *. r

let a2 = area 2.0

(* redefine pi*)
let pi = 6.0

(*what happens here?*)
let a3 = area 2.0
