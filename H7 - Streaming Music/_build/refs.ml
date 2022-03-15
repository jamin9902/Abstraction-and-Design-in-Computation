(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                   Part 1: Mutable Lists and Cycles
 *)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref ;;

 (*......................................................................
Problem 1: Write a function `has_cycle` that returns `true` if a
mutable list has a cycle, `false` otherwise. You may want a recursive
auxiliary function. Don't worry about space usage.

For instance, we can establish a cyclic and an acyclic mutable list
like this:

    # let sample_end = ref Nil ;;
    # let cyclic = Cons (1, ref (Cons (2, sample_end))) ;;
    # sample_end := cyclic ;;
    # let acyclic = Cons (3, ref (Cons(4, ref Nil))) ;;

and test for cycles using `has_cycle`:

    # has_cycle cyclic ;;
    - : bool = true
    # has_cycle acyclic ;;
    - : bool = false
  ......................................................................*)

let has_cycle (lst : 'a mlist) : bool =
  let rec aux (mlst : 'a mlist) (lst : 'a mlist ref list) : bool =
    match mlst with
    | Nil -> false
    | Cons (_ , x) -> List.exists ((==) x) lst || aux !x (x :: lst) in
  aux lst [] ;;

(*......................................................................
Problem 2: Write a function `flatten` that flattens a list (removes
its cycles if it has any) destructively. Again, you may want a
recursive auxiliary function, and you shouldn't worry about space.
  ......................................................................*)

let flatten (lst : 'a mlist) : unit =
  let rec aux (mlst : 'a mlist) (lst : 'a mlist ref list) =
    match mlst with
    | Nil -> ()
    | Cons (_ , x) ->
        match !x with
        | Nil -> ()
        | Cons (_, { contents = Nil }) -> ()
        | Cons (_, x2) ->
            if List.exists ((==) x2) lst then (x := Nil)
            else aux !x (x :: lst) in
  aux lst [] ;;

(*......................................................................
Problem 3: Write a function `mlength`, which nondestructively finds
the number of nodes in a mutable list that may have cycles.
  ......................................................................*)

let mlength (lst : 'a mlist) : int =
  let rec aux (mlst : 'a mlist) (lst : 'a mlist ref list) (count : int) : int =
    match mlst with
    | Nil -> count
    | Cons (_ , x) ->
        if List.exists ((==) x) lst then count
        else aux !x (x :: lst) (count + 1) in
  aux lst [] 0 ;;

(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set (in total, not just this file) took you to
complete. (If you worked with a partner, we're asking for how much time
each of you (on average) spent on the problem set, not in total.)
......................................................................*)

let minutes_spent_on_pset () : int = 420 ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
......................................................................*)

let reflection () : string =
  "It was hard to conceptualize the functions involving infinite trees, it would"
  ^ "have helped to draw out some of the functions before attempting to write them.";;
