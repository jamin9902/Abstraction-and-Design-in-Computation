(*
                         CS 51 Problem Set 2
            Higher Order Functional Programming -- Testing
 *)

open Bignum ;;

(* unit_test test msg -- Returns unit, with side effect of printing a
   report identified by msg on whether the unit test passed (returned
   true) or failed (returned false) *)
let unit_test (condition : bool) (msg : string) : unit =
  if condition then
    Printf.printf "%s passed\n" msg
  else
    Printf.printf "%s FAILED\n" msg ;;

let test () =
  unit_test (negate {neg = false; coeffs = []} = {neg = false; coeffs = []})
             "negate empty";
  unit_test (negate {neg = false; coeffs = [1]} = {neg = true; coeffs = [1]})
             "negate positive";
  unit_test (negate {neg = true; coeffs = [1; 2; 3]} = {neg = false; coeffs = [1; 2; 3]})
             "negate negative";

  unit_test (equal {neg = false; coeffs = []} {neg = false; coeffs = []} = true)
             "equal zero";
  unit_test (equal {neg = false; coeffs = [1]} {neg = true; coeffs = [1]} = false)
             "equal different signs";
  unit_test (equal {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1]} = false)
             "equal different lengths";
  unit_test (equal {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 3]} = false)
             "equal different coeffs";
  unit_test (equal {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]} = true)
             "equal equal";

  unit_test (less {neg = false; coeffs = []} {neg = false; coeffs = []} = false)
             "less zero equal";
  unit_test (less {neg = false; coeffs = [1]} {neg = false; coeffs = [1]} = false)
             "less equal";
  unit_test (less {neg = false; coeffs = [1]} {neg = false; coeffs = [1; 2]} = true)
             "less different lengths less";
  unit_test (less {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1]} = false)
             "less different lengths greater";
  unit_test (less {neg = true; coeffs = [1]} {neg = false; coeffs = [1]} = true)
             "less different signs less";
  unit_test (less {neg = false; coeffs = [1]} {neg = true; coeffs = [1]} = false)
             "less different signs greater";
  unit_test (less {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 3]} = true)
             "less different coeffs less";
  unit_test (less {neg = false; coeffs = [1; 3]} {neg = false; coeffs = [1; 2]} = false)
             "less different coeffs greater";
  unit_test (less {neg = true; coeffs = [1; 3]} {neg = true; coeffs = [1; 2]} = true)
             "less negative";

  unit_test (greater {neg = false; coeffs = []} {neg = false; coeffs = []} = false)
             "greater zero equal";
  unit_test (greater {neg = false; coeffs = [1]} {neg = false; coeffs = [1]} = false)
             "greater equal";
  unit_test (greater {neg = false; coeffs = [1]} {neg = false; coeffs = [1; 2]} = false)
             "greater different lengths less";
  unit_test (greater {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1]} = true)
             "greater different lengths greater";
  unit_test (greater {neg = true; coeffs = [1]} {neg = false; coeffs = [1]} = false)
             "greater different signs less";
  unit_test (greater {neg = false; coeffs = [1]} {neg = true; coeffs = [1]} = true)
             "greater different signs greater";
  unit_test (greater {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 3]} = false)
             "greater different coeffs less";
  unit_test (greater {neg = false; coeffs = [1; 3]} {neg = false; coeffs = [1; 2]} = true)
             "greater different coeffs greater";
  unit_test (greater {neg = true; coeffs = [1; 2]} {neg = true; coeffs = [1; 3]} = true)
             "greater negative";

  unit_test (from_int 0 = {neg = false; coeffs = []})
             "from_int zero";
  unit_test (from_int 123456789 = {neg = false; coeffs = [123; 456; 789]})
             "from_int positive";
  unit_test (from_int (-987654321) = {neg = true; coeffs = [987; 654; 321]})
             "from_int negative";
  unit_test (from_int max_int = {neg = false; coeffs = [4; 611; 686; 18; 427; 387; 903]})
             "from_int max_int";
  unit_test (from_int min_int = {neg = true; coeffs = [4; 611; 686; 18; 427; 387; 904]})
             "from_int min_int";

  unit_test (to_int {neg = false; coeffs = []} = Some 0)
             "to_int zero";
  unit_test (to_int {neg = false; coeffs = [123; 456; 789]} = Some 123456789)
             "to_int positive";
  unit_test (to_int {neg = true; coeffs = [987; 654; 321]} = Some (-987654321))
             "to_int negative";
  unit_test (to_int {neg = false; coeffs = [4; 611; 686; 18; 427; 387; 903]} = Some max_int)
             "to_int max_int";
  unit_test (to_int {neg = true; coeffs = [4; 611; 686; 18; 427; 387; 904]} = Some min_int)
             "to_int min_int";
  unit_test (to_int {neg = false; coeffs = [4; 611; 686; 18; 427; 387; 904]} = None)
             "to_int beyond max_int";
  unit_test (to_int {neg = true; coeffs = [4; 611; 686; 18; 427; 387; 905]} = None)
             "to_int beyond min_int";
  unit_test (to_int {neg = false; coeffs = [7; 611; 686; 18; 427; 387; 904]} = None)
             "to_int way beyond max_int";
  unit_test (to_int {neg = true; coeffs = [7; 611; 686; 18; 427; 387; 905]} = None)
             "to_int way beyond min_int";

  unit_test (plus {neg = false; coeffs = []} {neg = false; coeffs = []} = {neg = false; coeffs = []})
             "plus zeros";
  unit_test (plus {neg = false; coeffs = []} {neg = false; coeffs = [1]} = {neg = false; coeffs = [1]})
             "plus zero";
  unit_test (plus {neg = false; coeffs = [1; 999]} {neg = false; coeffs = [1]} = {neg = false; coeffs = [2; 0]})
             "plus pos pos";
  unit_test (plus {neg = true; coeffs = [1; 999]} {neg = true; coeffs = [1]} = {neg = true; coeffs = [2; 0]})
             "plus neg neg";
  unit_test (plus {neg = true; coeffs = [2; 0]} {neg = false; coeffs = [1; 999]} = {neg = true; coeffs = [1]})
             "plus greater neg pos";
  unit_test (plus {neg = true; coeffs = [1; 999]} {neg = false; coeffs = [2; 0]} = {neg = false; coeffs = [1]})
             "plus neg greater pos";
  unit_test (plus {neg = false; coeffs = [2; 0]} {neg = true; coeffs = [1; 999]} = {neg = false; coeffs = [1]})
             "plus greater pos neg";
  unit_test (plus {neg = false; coeffs = [1; 999]} {neg = true; coeffs = [2; 0]} = {neg = true; coeffs = [1]})
             "plus pos greater neg";
  unit_test (plus {neg = false; coeffs = [1; 999]} {neg = true; coeffs = [1; 999]} = {neg = false; coeffs = []})
             "plus greater pos neg equal";
  unit_test (plus {neg = true; coeffs = [1; 999]} {neg = false; coeffs = [1; 999]} = {neg = false; coeffs = []})
             "plus neg pos equal";

  unit_test (times {neg = false; coeffs = [1]} {neg = false; coeffs = []} = {neg = false; coeffs = []})
             "times zero";
  unit_test (times {neg = false; coeffs = [1]} {neg = true; coeffs = [1]} = {neg = true; coeffs = [1]})
             "times neg";
  unit_test (times {neg = false; coeffs = [1; 0]} {neg = false; coeffs = [123]} = {neg = false; coeffs = [123; 0]})
             "times single";
  unit_test (times {neg = false; coeffs = [123]} {neg = false; coeffs = [123]} = {neg = false; coeffs = [15; 129]})
             "times carry";
  unit_test (times {neg = false; coeffs = [123; 456; 789]} {neg = false; coeffs = [123; 456; 789]}
             = {neg = false; coeffs = [15; 241; 578; 750; 190; 521]})
             "times big";


  () ;;

test ();;
