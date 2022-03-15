(*
                         CS 51 Problem Set 2
                 Higher-Order Functional Programming
 *)

(*======================================================================
Before reading this code (or in tandem), read the problem set 2
writeup in the textbook. It provides context and crucial information
for completing the problems. In addition, make sure that you are
familiar with the problem set procedures in the document "Problem set
procedures for CS51".

The goal of this problem set is to get you programming with
higher-order functions in the "map/fold/filter" style. Consequently,
your solutions should all use `List.map`, `List.fold_left`,
`List.fold_right`, `List.filter`, or functions from earlier
problems. As a result, you should not need to change the definition of
these functions to make them explicitly recursive. You are permitted
to use other functions from the `List` module; however, you should
think about whether they're necessary and be mindful of their usage.

A solution that does not use at least one of these higher-order
functions, even a working one, will receive little or no design and
style credit. However, if you can express your solution to a
particular problem in terms of other functions from earlier problems,
you may do so.

........................................................................
Problem 1: Define a function `negate_all` that flips the sign of each
element in an integer list.
......................................................................*)

let negate_all (nums : int list) : int list =
  List.map (( * ) (-1)) nums ;;

(*......................................................................
Problem 2: Define a function `sum` that returns the sum of the
elements in an integer list.
......................................................................*)

let sum (nums : int list) : int =
  List.fold_left (+) 0 nums ;;

(*......................................................................
Problem 3: Define a function `sum_rows` that takes a list of "rows",
each an `int list` and returns a one-dimensional `int list`. Each `int`
in this list is equal to the sum of the corresponding rows in the
input. For example:

    # sum_rows [[1; 2]; [3; 4]] ;;
    - : int list = [3; 7]
......................................................................*)

let sum_rows (rows : int list list) : int list =
  List.map sum rows ;;

(*......................................................................
Problem 4: Define a function `filter_odd` that takes an integer list and
retains only the odd numbers from the given list. For example:

    # filter_odd [1; 4; 5; -3] ;;
    - : int list = [1; 5; -3]
......................................................................*)

let filter_odd (nums : int list) : int list =
  List.filter (fun x -> x mod 2 <> 0) nums ;;

(*......................................................................
Problem 5: Define a function `num_occurs` that returns the number of
times a given number appears in a list. For example:

    # num_occurs 4 [1; 3; 4; 5; 4] ;;
    - : int = 2
......................................................................*)

let num_occurs (n : int) (nums : int list) : int =
  List.length (List.filter (fun x -> x = n) nums) ;;

(*......................................................................
Problem 6: Define a function `super_sum` that sums all of the numbers in
a list of integer lists. For example:

# super_sum [[1; 2; 3]; []; [5]] ;;
- : int = 11
......................................................................*)

let super_sum (nlists : int list list) : int =
  sum (sum_rows nlists) ;;

(*......................................................................
Problem 7: Define a function `filter_range` that takes a list `lst` and
a pair of integers defining lower and upper bounds on a range and
returns a list of integers in the input list within the given range
(inclusive), in the same order they appeared in the input list. For
example:

    # filter_range [1; 3; 4; 5; 2] (1, 3) ;;
    - : int list = [1; 3; 2]
    # filter_range [1; 3; 4; 5; 2] (3, 2) ;;
    - : int list = []

Note the last example, which shows what happens when the lower bound
is higher than the upper bound. In that case, no numbers can fall
within the range, and the result is the empty list.
......................................................................*)

let filter_range (nums : int list) (range : int * int) : int list =
  let (mn, mx) = range in
  List.filter (fun x -> x >= mn && x <= mx) nums ;;

(*......................................................................
Problem 8: Define a function `floats_of_ints` that converts an `int
list` into a `float list`. For example:

    # floats_of_ints [1; 2; 3] ;;
    - : float list = [1.; 2.; 3.]
......................................................................*)

let floats_of_ints (nums : int list) : float list =
  List.map (float_of_int) nums ;;

(*......................................................................
Problem 9: Define a function `log10s` that applies the `log10` function
to all members of a list of floats, returning a list of `float
option`s. Since the function `log10` is not defined for numbers n <= 0,
undefined results should be `None`. For example:

    # log10s [1.0; 10.0; -10.0] ;;
    - : float option list = [Some 0.; Some 1.; None]
......................................................................*)

let log10s (lst: float list) : float option list =
  List.map (fun x -> if x <= 0. then None else Some (log10 x)) lst;;

(*......................................................................
Problem 10: Define a function `deoptionalize` that extracts values from
a list of options, ignoring `None` values. For example:

    # deoptionalize [Some 3; None; Some 5; Some 10] ;;
    - : 'a list = [3; 5; 10]
......................................................................*)

let deoptionalize (lst : 'a option list) : 'a list =
  List.filter_map (fun x -> x) lst ;;

(*......................................................................
Problem 11: Define a function `some_sum` that sums all of the numbers in
a list of `int option`s but ignores `None` values. For example:

    # some_sum [Some 3; None; Some 5; Some 10] ;;
    - : int = 18
......................................................................*)

let some_sum (nums : int option list) : int =
  sum (deoptionalize nums) ;;

(*......................................................................
Problem 12: Define a function `mult_odds` that returns the product of
all of the odd elements of a list. For example:

    # mult_odds [1; 3; 0; 2; -5] ;;
    - : int = -15

NOTE: See the multiplicative identity
(https://en.wikipedia.org/wiki/Identity_element#Definitions) if you're
wondering what to do in a certain edge case.
......................................................................*)

let mult_odds (nums : int list) : int =
  List.fold_left ( * ) 1 (filter_odd nums) ;;

(*......................................................................
Problem 13: Define a function `concat` that concatenates a list of
lists. For example:

    # concat [[1; 2]; []; [3; 4; 5]; [6]] ;;
    - : int list = [1; 2; 3; 4; 5; 6]
......................................................................*)

let concat (lists : 'a list list) : 'a list =
  List.fold_left (@) [] lists ;;

(* For the next problem, we define a type that represents a student
as a tuple of the student's name and year. *)

type name = string ;;
type year = int ;;
type student = name * year ;;

(*......................................................................
Problem 14: Define a function `filter_by_year` that returns the names of
all the students in a given year. For example:

    # let students = [("Joe", 2010); ("Bob", 2010); ("Tom", 2013)] ;;
    val students : (string * int) list =
      [("Joe", 2010); ("Bob", 2010); ("Tom", 2013)]

    # filter_by_year students 2010 =
    - : name list = ["Joe"; "Bob"]
......................................................................*)

let filter_by_year (slist : student list) (yr : year) : name list =
  List.map (fun (n, _) -> n) (List.filter (fun (_, y) -> y = yr) slist);;

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
  "I ran into problems considering the benefits and downsides of using"
  ^ "different functions from the 'List' module. I wasn't sure whether"
  ^ "to prioritze only using map, fold, and filter or above all else or"
  ^ "or whether to use other 'List' modules functions whenever convenient."
  ^ "I ended up prioritzing clarity of my code and used other functions"
  ^ "whenever they greatly reduced the length of my code. I could have"
  ^ "reduced the time spent deliberating on this question of priorities"
  ^ "by attending an office hours to ask the question but unfortunately,"
  ^ "I was not available for any office hours this week because of Chinese"
  ^ "New Year celebrations." ;;
