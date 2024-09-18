(*Week 3: Induction*)

(*recursive tree data type *)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a  tree

(*an n-ary tree*)

type 'a nary_tree = Empty | Node of 'a * ('a tree) list

  

