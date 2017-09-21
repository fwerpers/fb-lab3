(* Tail recursion and specification *)

(* average x
   TYPE: real list -> real
   PRE: true
   POST: average of elements in x
   EXAMPLE: average [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] = 3.5
*)
fun average x =
let
  (* accumulate s n x
     TYPE: real -> int -> real list -> real
     PRE: true
     POST: average of element in x
  *)
  (* VARIANT: length of x *)
  fun accumulate _ 0 [] = real(0)
    | accumulate sum_acc n_acc [] = sum_acc / real(n_acc) 
    | accumulate sum_acc n_acc (next::rest) = accumulate (sum_acc + next) (n_acc + 1) rest
in
   accumulate 0.0 0 x
end

(* Higher-order functions *)

(* append x y
   TYPE: 'a list -> 'a list -> 'a list
   PRE: true
   POST: 
   EXAMPLES: append [1, 2, 3] [5, 6] = [1, 2, 3, 4, 5]
*)
fun append x y = foldr (op ::) y x

(* member a x
   TYPE: ''a -> ''a list -> bool
   PRE: true
   POST: is a an element in x
   EXAMPLES: member 3 [1, 2, 3, 4, 5] = true
*)
fun member a x = foldl (fn (y, z) => z orelse y = a) false x

(* last x
   TYPE: 'a list -> 'a
   PRE: true
   POST: last element of x
   EXAMPLES: last [1, 2, 3] = 3 
*)
fun last [] = raise Empty
  | last (x::xs) = foldl (fn (y, z) => y) x xs 

(* reverse x
   TYPE: 'a list -> 'a list
   PRE: true
   POST: list with elements of x in reverse order
   EXAMPLES: reverse [1, 2, 3] = [3, 2, 1]
*)
fun reverse x = foldl (op ::) [] x

(* filter f x
   TYPE: ('a -> bool) -> 'a list -> 'a list
   PRE: true
   POST: a list of all element from x fulfilling predicate f
*)
fun filter f x = foldr (fn (y, z) => if f y then y::z else z) [] x

(* Binary search trees and complexity *)

(* REPRESENTATION CONVENTION: generic
   REPRESENTATION INVARIANT: left subtree smaller than node and right subtree larger
*)
datatype tree = Void | Node of tree * int * tree

(* sub_tree a b tree
   TYPE: int -> int -> tree -> tree
   PRE: true
   POST: subtree of all nodes with a <= label < b
*)
fun sub_tree _ _ (Void) = Void
  | sub_tree a b (Node(left, label, right)) = 
    if label >= a andalso label < b then
       Node((sub_tree a b left), label, (sub_tree a b right))
    else if label >= a then
       (sub_tree a b left)
    else if label < b then
       (sub_tree a b right)
    else
       Void

(* Complexity of sub_tree ... *)