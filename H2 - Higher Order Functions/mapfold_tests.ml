(*
                         CS 51 Problem Set 2
            Higher Order Functional Programming -- Testing
 *)

open Mapfold ;;

open Test_simple ;;      (* a really simple unit testing framework *)

let test () =
  unit_test (negate_all [] = [])
            "negate_all empty";
  unit_test (negate_all [1; -2; 0] = [-1; 2; 0])
            "negate_all mixed";
  unit_test (sum [] = 0)
            "sum empty";
  unit_test (sum [1; 2; 0; -7] = -4)
            "sum mixed";
  unit_test (sum_rows [] = [])
            "sum_rows empty";
  unit_test (sum_rows [[]; []] = [0; 0])
            "sum_rows empty rows";
  unit_test (sum_rows [[]; [3; 4; 1]; [-3]; [1; -5]]
              = [0; 8; -3; -4])
            "sum_rows mixed";
  unit_test (filter_odd [] = [])
            "filter_odd empty";
  unit_test (filter_odd [-2; 4; -10; 28] = [])
            "filter_odd evens";
  unit_test (filter_odd [-3; 5; -19; 23] = [-3; 5; -19; 23])
            "filter_odd odds";
  unit_test (filter_odd [-1; 0; -18; 33] = [-1; 33])
            "filter_odd mixed";
  unit_test (num_occurs 1 [] = 0)
            "num_occurs empty";
  unit_test (num_occurs 1 [0; 2; 5] = 0)
            "num_occurs no occurence";
  unit_test (num_occurs 1 [1; 1; 0; -7; -1; 11] = 2)
            "num_occurs positive";
  unit_test (num_occurs (-1) [1; -1; 3; 0; -1; 5] = 2)
            "num_occurs negative";
  unit_test (super_sum [] = 0)
            "super_sum empty";
  unit_test (super_sum [[]; []] = 0)
            "super_sum empty rows";
  unit_test (super_sum [[]; [0]; [3; 4; 1]; [-3]; [1; -5]] = 1)
            "super_sum mixed";
  unit_test (filter_range [] (0, 0) = [])
            "filter_range empty";
  unit_test (filter_range [1; 0; 1; 5] (0, 0) = [0])
            "filter_range duplicate range";
  unit_test (filter_range [2; -1; 3] (0, 1) = [])
            "filter_range not in range";
  unit_test (filter_range [1; 0; 9; -11] (1, 0) = [])
            "filter_range inverted range";
  unit_test (filter_range [-11; -1; 0; 3; 9; -11] (-1, 3) = [-1; 0; 3])
            "filter_range mixed";
  unit_test (floats_of_ints [] = [])
            "floats_of_ints empty";
  unit_test (floats_of_ints [(1/2); 0; -4; 33; -102]
             = [0.; 0.; -4.; 33.; -102.])
            "floats_of_ints mixed";
  unit_test (log10s [] = [])
            "log10s empty";
  unit_test (log10s [1.; 0.1; 10.; -10.; 0.]
             = [Some 0.; Some (-1.); Some 1.; None; None])
            "log10s mixed";
  unit_test (deoptionalize [] = [])
            "deoptionalize empty";
  unit_test (deoptionalize [Some (-3); None; Some 5; Some 10] = [-3; 5; 10])
            "deoptionalize mixed";
  unit_test (some_sum [] = 0)
            "some_sum empty";
  unit_test (some_sum [None; None] = 0)
            "some_sum none";
  unit_test (some_sum [Some (-3); None; Some 5; Some 10] = 12)
            "deoptionalize mixed";
  unit_test (mult_odds [] = 1)
            "mult_odds empty";
  unit_test (mult_odds [2; 4; 10] = 1)
            "mult_odds evens";
  unit_test (mult_odds [-1; 3; -15] = 45)
            "mult_odds odds";
  unit_test (mult_odds [-1; 7; 6; 2; 3] = -21)
            "mult_odds mixed";
  unit_test (concat [] = [])
              "concat empty";
  unit_test (concat [[]; []] = [])
            "concat empty lists";
  unit_test (concat [[1]; [0; -4; 7] ; []; [9; 12]]
             = [1; 0; -4; 7; 9; 12])
            "concat mixed";
  unit_test (filter_by_year [] 0 = [])
            "filter_by_year empty";
  unit_test (filter_by_year
             [("Joe", 2010); ("Bob", 2010); ("Tom", 2013)] 0
             = [])
            "filter_by_year not found";
  unit_test (filter_by_year
           [("Joe", 2010); ("Bob", 2010); ("Tom", 2013)] 2010
            = ["Joe"; "Bob"])
           "filter_by_year mixed";

  () ;;

test ();;
