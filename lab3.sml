(* Tail Recursion *)
(*  average l
    TYPE: real list -> real
    PRE: true
    POST: returns the average of the elemnts of l
*)
(* VARIANT: length of l *)
fun average [] = 0.0
  | average l =
    let
        fun helper sum n [] = sum/real(n)
          | helper sum n (x::xs) = helper (sum + x) (n+1) xs;
    in
        helper 0.0 0 l
    end;

(* Use of Higher-Order Functions *)
(* See http://sml-family.org/Basis/list.html#LIST:SIG:SPEC *)
fun append l1 l2 = foldr (fn (head, tail) => head::tail) l2 l1;
fun member a b = true;
fun last (x::xs) = x;
fun reverse l = l;
fun filter f l = l;

(* Binary Search Trees *)
datatype tree = Void | Node of tree * int * tree;

fun sub_tree a b t = t;

(* Complexity *)
