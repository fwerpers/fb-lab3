(* Tail Recursion *)
(*  average l
    TYPE: real list -> real
    PRE: true
    POST: returns the average of the elemnts of l
*)
fun average l = 1.0;

(* Use of Higher-Order Functions *)
fun append a b = [];
fun member a b = true;
fun last (x::xs) = x;
fun reverse l = l;
fun filter f l = l;

(* Binary Search Trees *)
datatype tree = Void | Node of tree * int * tree;

fun sub_tree a b t = t;

(* Complexity *)
