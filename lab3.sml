(* Tail Recursion *)
(*  average l
    TYPE: real list -> real
    PRE: true
    POST: returns the average of the elemnts of l
*)
(*  VARIANT: length of l *)
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

fun member a l = foldr (fn (head, tail) => head=a orelse tail) false l;

fun last (x::xs) = foldl (fn (head, tail) => head) x xs;

fun reverse l = foldl (fn (head, tail) => head::tail) [] l;

fun filter f l = foldr (fn (head, tail) => if f head then head::tail else tail) [] l;

(* Binary Search Trees *)
datatype tree = Void | Node of tree * int * tree;

(*
Idea 1: Build an un-balanced tree "in-argument"
How would we do this?
Idea 2: Have a build_tree function that builds an un-balanced tree.
This would have to recurse down the tree every time we add a key.
*)
(* What's the variant? *)

(* val ex1 = Node(Node(Node(Void, 0, Node(Void, 2, Void)), 3, Node(Void, 5, Void)), 6, Node(Void, 7, Node(Void, 8, Node(Void, 9, Node(Node(Void, 10, Void), 12, Node(Void, 15, Node(Void, 19, Void))))))) *)

(* fun combine tree merge_tree =
let
    fun helper Void = merge_tree
      | helper (Node (Void, key, right)) = Node (merge_tree, key, right)
      | helper (Node (left, key, Void)) = Node (left, key, merge_tree)
      | helper (Node (left, key, right)) = helper left
in
    helper tree
end; *)

(* fun combine tree merge_tree =
let
    fun helper Void = merge_tree
      | helper (Node (left, key, right)) = Node (helper left, key, right)
in
    helper tree
end; *)

fun combine merge_tree Void = merge_tree
  | combine merge_tree (Node (left, key, right)) = Node (combine merge_tree left, key, right);

fun sub_tree a b t =
let
    fun helper Void = Void
      | helper (Node (left, key, right)) = 
        if a <= key andalso key < b then
            (* Return current node with left and right subtree *)
            Node (helper left, key, helper right)
        else
            (* Return combination of left and right subtree without current node *)
            combine (helper left) (helper right)
in
    helper t
end;

(* Complexity *)
