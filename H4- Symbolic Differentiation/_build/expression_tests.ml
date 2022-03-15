(*
                         CS 51 Problem Set 4
                 A Language for Symbolic Mathematics
                               Testing
 *)

open Expression ;;
open ExpressionLibrary ;;

open CS51Utils ;;
open Absbook ;;

let test () =
  unit_test (not (contains_var (parse "2"))) "contains_var number";
  unit_test (contains_var (parse "x")) "contains_var var";
  unit_test (contains_var (parse "sin(x)")) "contains_var var unop";
  unit_test (not (contains_var (parse "ln(5)"))) "contains_var number unop";
  unit_test (contains_var (parse "x + 4")) "contains_var var binop";
  unit_test (not (contains_var (parse "2 + 3"))) "contains_var number binop";

  unit_test_within 0.0001 (evaluate (parse "2") 1.) 2. "evaluate number";
  unit_test_within 0.0001 (evaluate (parse "x") (-.1.)) (-.1.) "evaluate var";
  unit_test_within 0.0001 (evaluate (parse "sin(x)") 1.) 0.8414709848 "evaluate sin";
  unit_test_within 0.0001 (evaluate (parse "cos(x)") 1.) 0.54030230586 "evaluate cos";
  unit_test_within 0.0001 (evaluate (parse "ln(x)") 2.) 0.69314718056 "evaluate ln";
  unit_test_within 0.0001 (evaluate (parse "~x") 1.) (-.1.) "evaluate neg";
  unit_test_within 0.0001 (evaluate (parse "x + 5") 1.) 6. "evaluate add";
  unit_test_within 0.0001 (evaluate (parse "x - 5") 1.) (-.4.) "evaluate sub";
  unit_test_within 0.0001 (evaluate (parse "x * 5") 1.) 5. "evaluate mul";
  unit_test_within 0.0001 (evaluate (parse "x / 5") 1.) 0.2 "evaluate div";
  unit_test_within 0.0001 (evaluate (parse "x^5") 2.) 32. "evaluate pow";

  unit_test ((to_string_smart (derivative (parse "2"))) = "0.") "derivative num";
  unit_test ((to_string_smart (derivative (parse "x"))) = "1.") "derivative var";
  unit_test ((to_string_smart (derivative (parse "sin(x)"))) = "cos(x)*1.") "derivative sin";
  unit_test ((to_string_smart (derivative (parse "cos(x)"))) = "~(sin(x))*1.") "derivative cos";
  unit_test ((to_string_smart (derivative (parse "ln(x)"))) = "1./x") "derivative pow ln";
  unit_test ((to_string_smart (derivative (parse "~(x)"))) = "~(1.)") "derivative pow neg";
  unit_test ((to_string_smart (derivative (parse "x + 1"))) = "1.+0.") "derivative add";
  unit_test ((to_string_smart (derivative (parse "x - 2"))) = "1.-0.") "derivative sub";
  unit_test ((to_string_smart (derivative (parse "x * 2"))) = "1.*2.+x*0.") "derivative mul";
  unit_test ((to_string_smart (derivative (parse "x / 2"))) = "(1.*2.-x*0.)/2.^2.") "derivative div";
  unit_test ((to_string_smart (derivative (parse "x^2"))) = "(2.*1.)*x^(2.-1.)") "derivative pow simple";
  unit_test ((to_string_smart (derivative (parse "x^x"))) = "x^x*(1.*ln(x)+(1.*x)/x)") "derivative pow complex";

  unit_test ((find_zero (parse "x") 3. 0.1 10) = Some 0.) "find_zero found";
  unit_test ((find_zero (parse "x^2") 100000. 0.1 2) = None) "find_zero not found in time";

  () ;;

test ();;
