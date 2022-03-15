(*
                         CS 51 Problem Set 4
                 A Language for Symbolic Mathematics
 *)

(*======================================================================
Before working on this problem set, read the problem set 4 writeup. It
provides context and crucial information for completing the
problems. In addition, make sure that you are familiar with the
problem set procedures in the document "Problem set procedures for
CS51".

We provide a type definition whose values represent arithmetic
expressions over floating point numbers with a single variable. The
type definition can be found at the top of `expressionLibrary.ml`,
along with enumerated type definitions for the unary and binary
operators, and other useful functions. You will be using this
algebraic data type for this part of the problem set. We refer to
these arithmetic expressions (represented by the `expression` type) as
the "object language". It is the object of our interest, the object of
our software's manipulation, as distinct from the "metalanguage"
(OCaml) in which our manipulations are written.

The module `ExpressionLibrary` is opened here to provide you with
access to the `expression` data type and helpful functions that you
will use for this part of the problem set.
......................................................................*)

open ExpressionLibrary ;;

(*......................................................................
Tips:

1. READ THE WRITEUP, particularly for the definition of the derivative
   function.

2. Use the type definitions provided at the top of
   `expressionLibrary.ml` as a reference, and don't change any of the
   code in that file. It provides functions such as `parse` and
   `to_string_smart` that will be helpful in this problem set.
......................................................................*)

(*......................................................................
Problem 1: The function `contains_var` tests whether an expression
contains a variable `x`. For example:

# contains_var (parse "x^4") ;;
- : bool = true
# contains_var (parse "4+3") ;;
- : bool = false
......................................................................*)

let rec contains_var (e : expression) : bool =
  match e with
  | Num _ -> false
  | Var -> true
  | Unop (_, e1) -> contains_var e1
  | Binop (_, e1, e2) -> contains_var e1 || contains_var e2;;

(*......................................................................
Problem 2: The function `evaluate` evaluates an expression for a
particular value of `x`. Don't worry about specially handling the
"divide by zero" case. For example:

# evaluate (parse "x^4 + 3") 2.0
- : float = 19.0
......................................................................*)

let rec evaluate (e : expression) (x : float) : float =
  match e with
  | Num n -> n
  | Var -> x
  | Unop (u, e1) ->
     let v1 = evaluate e1 x in
     (match u with
      | Sin -> sin v1
      | Cos -> cos v1
      | Ln -> log v1
      | Neg -> -. v1)
  | Binop (b, e1, e2) ->
     let v1, v2 = evaluate e1 x, evaluate e2 x in
     match b with
      | Add -> v1 +. v2
      | Sub -> v1 -. v2
      | Mul -> v1 *. v2
      | Div -> v1 /. v2
      | Pow -> v1 ** v2 ;;

(*......................................................................
Problem 3: The `derivative` function returns the expression that
represents the derivative of the argument expression. We provide the
skeleton of the implementation here along with a few of the cases;
you're responsible for filling in the remaining parts that implement
the derivative transformation provided in the figure in the
writeup. See the writeup for details.
......................................................................*)

let rec derivative (e : expression) : expression =
  match e with
  | Num _ -> Num 0.
  | Var -> Num 1.
  | Unop (u, e1) ->
     (match u with
      | Sin -> Binop (Mul, Unop (Cos, e1), derivative e1)
      | Cos -> Binop (Mul, Unop (Neg, Unop (Sin, e1)), derivative e1)
      | Ln ->  Binop (Div, derivative e1, e1)
      | Neg -> Unop (Neg, derivative e1))
  | Binop (b, e1, e2) ->
     match b with
     | Add -> Binop (Add, derivative e1, derivative e2)
     | Sub -> Binop (Sub, derivative e1, derivative e2)
     | Mul -> Binop (Add, Binop (Mul, derivative e1, e2),
                          Binop (Mul, e1, derivative e2))
     | Div -> Binop (Div, Binop (Sub, Binop (Mul, derivative e1, e2),
                                      Binop (Mul, e1, derivative e2)),
                          Binop (Pow, e2, Num 2.))
     | Pow ->
        match e2 with
        | Num _ -> Binop (Mul, Binop (Mul, e2, derivative e1),
                          Binop (Pow, e1, Binop (Sub, e2, Num 1.)))
        | _ -> Binop (Mul, Binop (Pow, e1, e2),
                      Binop (Add, Binop (Mul, derivative e2, Unop(Ln, e1)),
                             Binop (Div, Binop (Mul, derivative e1, e2), e1))) ;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
  print_string ("Checking expression: " ^ strs ^ "\n");
  let parsed = parse strs in
  (print_string "contains variable : ";
   print_string (string_of_bool (contains_var parsed));
   print_endline " ";
   print_string "Result of evaluation: ";
   print_float (evaluate parsed xval);
   print_endline " ";
   print_string "Result of derivative: ";
   print_endline " ";
   print_string (to_string (derivative parsed));
   print_endline " ") ;;

(*......................................................................
Problem 4: Zero-finding. See writeup for instructions.
  ......................................................................*)

let rec find_zero (expr : expression)
              (guess : float)
              (epsilon : float)
              (limit : int)
              : float option =
  match limit with
  | 0 -> None
  | _ ->
      let val_guess = evaluate expr guess in
      if abs_float (val_guess) < epsilon then Some guess
      else
        let new_guess = guess -. val_guess /. (evaluate (derivative expr) guess)
        in find_zero expr new_guess epsilon (limit - 1) ;;

(*......................................................................
Problem 5: Challenge problem -- exact zero-finding. This problem is
not counted for credit and is not required. Just leave it
unimplemented if you do not want to do it. See writeup for
instructions.
......................................................................*)

let find_zero_exact (e : expression) : expression option =
  failwith "find_zero_exact not implemented" ;;

(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set took you to complete.
......................................................................*)

let minutes_spent_on_pset () : int = 300 ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set. Where
did you run into problems and how did you end up resolving them? What
might you have done in retrospect that would have allowed you to
generate as good a submission in less time? Please provide us your
thoughts on these questions and any other reflections in the string
below.
......................................................................*)

let reflection () : string =
  "My code for certain sections of derivative looked extremeley messy"
  ^ "which made it hard to navigate and check through. I added more"
  ^ "levels of indentation to help make the code more readable.";;
