(*
                         CS 51 Problem Set 1
                Core Functional Programming -- Testing
*)

open Ps1 ;;

(* The Absbook module contains simple functions for unit testing:
   `unit_test` and `unit_test_within`. *)
open CS51Utils ;;
open Absbook ;;

let nonincreasing_test () =
  unit_test (nonincreasing [])
            "nonincreasing empty" ;
  unit_test (nonincreasing [7])
            "nonincreasing single" ;
  unit_test (nonincreasing [4;4])
            "nonincreasing double" ;
  unit_test (not (nonincreasing [1;2]))
            "nonincreasing inc double" ;
  unit_test (nonincreasing [4;4;4])
            "nonincreasing repeat" ;
  unit_test (not (nonincreasing [1;2;3]))
            "nonincreasing inc" ;
  unit_test (not (nonincreasing [2;1;2]))
            "nonincreasing inc after start" ;
  unit_test (nonincreasing [2;2;1])
            "nonincreasing dups" ;
  unit_test (nonincreasing [9;8;7;6;5;5;5;4;4;-2])
            "nonincreasing long with neg" ;
  unit_test (not (nonincreasing [9;8;7;6;7;5;5;5;5;4;3]))
    "nonincreasing long inc at mid" ;;

let merge_test () =
  unit_test (merge [] [] = [])
            "merge empty" ;
  unit_test (merge [1] [] = [1])
            "merge left single empty" ;
  unit_test (merge [] [1] = [1])
            "merge right single empty" ;
  unit_test (merge [1] [1] = [1; 1])
            "merge dup" ;
  unit_test (merge [1; 3; 5] [2; 4; 6; 7; 12]
             = [1; 2; 3; 4; 5; 6; 7; 12])
            "merge different length" ;
  unit_test (merge [1; 3; 5] [2; 4; 6] = [1; 2; 3; 4; 5; 6])
    "merge normal" ;;

let unzip_test () =
  unit_test (unzip [] = ([], []))
    "unzip empty" ;
  unit_test (unzip [(true, false)] = ([true], [false]))
    "unzip single" ;
  unit_test (unzip [(true, false); (true, true)]
             = ([true; true], [false; true]))
    "unzip double" ;
  unit_test (unzip [(true, false); (false, false); (true, false)]
             = ([true; false; true], [false; false; false]))
    "unzip multiple" ;;

let test_to_run_length () =
  unit_test (to_run_length [] = [])
    "to_run_length empty" ;
  unit_test (to_run_length ['a'] = [(1, 'a')])
    "to_run_length single" ;
  unit_test (to_run_length ['a'; 'a'; 'a'] = [(3, 'a')])
    "to_run_length single letter" ;
  unit_test (to_run_length ['a'; 'b'] = [(1, 'a'); (1, 'b')])
    "to_run_length double" ;
  unit_test (to_run_length ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd']
             = [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')])
    "to_run_length multiple" ;;

let test_from_run_length () =
  unit_test (from_run_length [] = [])
    "from_run_length empty" ;
  unit_test (from_run_length [(1, 'a')] = ['a'])
    "from_run_length single" ;
  unit_test (from_run_length [(3, 'a')] = ['a'; 'a'; 'a'])
    "from_run_length single letter" ;
  unit_test (from_run_length [(1, 'a'); (1, 'b')] = ['a'; 'b'])
    "from_run_length double" ;
  unit_test (from_run_length [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')]
             = ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd'])
    "from_run_length multiple" ;;

let test_extract_entry () =
  unit_test (extract_entry (cCOOPERATE, cCOOPERATE) test_payoff_matrix
             = (3, 3))
    "extract_entry test_payoff_matrix 1" ;
  unit_test (extract_entry (cCOOPERATE, cDEFECT) test_payoff_matrix
             = (-2, 5))
    "extract_entry test_payoff_matrix 2" ;
  unit_test (extract_entry (cDEFECT, cCOOPERATE) test_payoff_matrix
             = (5, -2))
    "extract_entry test_payoff_matrix 3" ;
  unit_test (extract_entry (cDEFECT, cDEFECT) test_payoff_matrix
             = (0, 0))
    "extract_entry test_payoff_matrix 4" ;;

let test_history_empty = [] ;;
let test_history_single = [(cCOOPERATE, cCOOPERATE)] ;;
let test_history_double = [(cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE)] ;;
let test_history_multiple = [(cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE)] ;;

let test_count_defections () =
  unit_test (count_defections test_history_empty = (0, 0))
    "count_defections empty" ;
  unit_test (count_defections test_history_single = (0, 0))
    "count_defections single" ;
  unit_test (count_defections test_history_double = (1, 0))
    "count_defections double" ;
  unit_test (count_defections test_history_multiple = (1, 1))
    "count_defections multiple" ;;

let test_count_cooperations () =
  unit_test (count_cooperations test_history_empty = (0, 0))
    "count_cooperations empty" ;
  unit_test (count_cooperations test_history_single = (1, 1))
    "count_cooperations single" ;
  unit_test (count_cooperations test_history_double = (1, 2))
    "count_cooperations double" ;
  unit_test (count_cooperations test_history_multiple = (2, 2))
    "count_cooperations multiple" ;;

let test_balanced () =
  unit_test (balanced test_history_empty = cCOOPERATE)
    "balanced empty" ;
  unit_test (balanced test_history_single = cDEFECT)
    "balanced single" ;
  unit_test (balanced test_history_double = cCOOPERATE)
    "balanced double" ;
  unit_test (balanced test_history_multiple = cDEFECT)
    "balanced multiple" ;;

let test_egalitarian () =
  unit_test (egalitarian test_history_empty = cCOOPERATE)
    "egalitarian empty" ;
  unit_test (egalitarian test_history_single = cCOOPERATE)
    "egalitarian single" ;
  unit_test (egalitarian test_history_double = cCOOPERATE)
    "egalitarian double" ;
  unit_test (egalitarian [(cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE)]
             = cDEFECT)
    "egalitarian defect" ;
  unit_test (egalitarian test_history_multiple = cCOOPERATE)
    "egalitarian multiple" ;;

let test_tit_for_tat () =
  unit_test (tit_for_tat test_history_empty = cCOOPERATE)
    "tit_for_tat empty" ;
  unit_test (tit_for_tat test_history_single = cCOOPERATE)
    "tit_for_tat single" ;
  unit_test (tit_for_tat test_history_double = cCOOPERATE)
    "tit_for_tat double" ;
  unit_test (tit_for_tat test_history_multiple = cDEFECT)
    "tit_for_tat multiple" ;;

let test_my_strategy () =
  unit_test (my_strategy test_history_empty = cCOOPERATE)
    "my_strategy empty" ;
  unit_test (my_strategy test_history_single = cCOOPERATE)
    "my_strategy single" ;
  unit_test (my_strategy test_history_double = cCOOPERATE)
    "my_strategy double" ;
  unit_test (my_strategy test_history_multiple = cDEFECT)
    "my_strategy multiple" ;
  unit_test (my_strategy
             [(cDEFECT, cCOOPERATE); (cCOOPERATE, cDEFECT); (cDEFECT, cCOOPERATE); (cCOOPERATE, cCOOPERATE)]
             = cDEFECT)
    "my_strategy multiple 2" ;;

let test_swap_actions () =
  unit_test (swap_actions test_history_empty
             = [])
    "swap_actions empty" ;
  unit_test (swap_actions test_history_single
             = [(cCOOPERATE, cCOOPERATE)])
    "swap_actions single" ;
  unit_test (swap_actions test_history_double
             = [(cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE)])
    "swap_actions double" ;
  unit_test (swap_actions test_history_multiple
             = [ (cDEFECT, cCOOPERATE); (cCOOPERATE, cDEFECT); (cCOOPERATE, cCOOPERATE)])
    "swap_actions multiple" ;;

let test_calculate_payoff () =
  unit_test (calculate_payoff test_payoff_matrix test_history_empty
             = (0, 0))
    "calculate_payoff empty" ;
  unit_test (calculate_payoff test_payoff_matrix test_history_single
             = (3, 3))
    "calculate_payoff single" ;
  unit_test (calculate_payoff test_payoff_matrix test_history_double
             = (8, 1))
    "calculate_payoff double" ;
  unit_test (calculate_payoff test_payoff_matrix test_history_multiple
             = (6, 6))
    "calculate_payoff multiple" ;;

let test_all () =
  test_from_run_length () ;;
  test_to_run_length () ;;
  unzip_test () ;;
  merge_test () ;;
  nonincreasing_test () ;;
  test_extract_entry () ;;
  test_count_cooperations () ;;
  test_count_defections () ;;
  test_balanced () ;;
  test_egalitarian () ;;
  test_tit_for_tat () ;;
  test_my_strategy () ;;
  test_swap_actions () ;;
  test_calculate_payoff () ;;

let _ = test_all () ;;
