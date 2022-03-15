open CS51Utils ;;
open Tiles ;;
open Mazes ;;
open Puzzledescription ;;
open Puzzlesolve ;;
open Collections ;;

(*......................................................................
                       SAMPLE TILE PUZZLE TESTING
*)

let _  = Random.init 0 ;;

let cDIMS = 2, 2 ;;
let solved : board =
  [| [|Tile 1; Tile 2;|];
     [|Tile 3; EmptyTile|]; |] ;;

let rand_elt l : board =
  fst (List.nth l (Random.int (List.length l))) ;;

let random_tileboard () : board =
  let cINITIAL_MOVE_COUNT = 45 in
  let module Puzzle : (PUZZLEDESCRIPTION with type state = Tiles.board
                                          and type move = Tiles.direction) =
    MakeTilePuzzleDescription (struct
      let initial = solved
      let dims = cDIMS
    end) in
  let rec make_moves n b =
    if n <= 0 then b
    else make_moves (n - 1) (rand_elt (Puzzle.neighbors b)) in
  make_moves cINITIAL_MOVE_COUNT Puzzle.initial_state ;;

let test_tile_puzzle () : unit =

  let module Puzzle : (PUZZLEDESCRIPTION with type state = Tiles.board
                                          and type move = Tiles.direction) =
    MakeTilePuzzleDescription
      (struct
        let initial = random_tileboard ()
        let dims = cDIMS
      end) in

  Printf.printf("TESTING RANDOMLY GENERATING 2x2 TILEPUZZLE...\n");
  assert (not (Puzzle.is_goal Puzzle.initial_state));

  let module DFSG = DFSSolver(Puzzle) in
  let module BFSG = BFSSolver(Puzzle) in
  let module FastBFSG = FastBFSSolver(Puzzle) in

  Printf.printf("Regular BFS time:\n");
  let (bfs_path, _bfs_expanded) =
    Absbook.call_reporting_time BFSG.solve () in
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves bfs_path));

  Printf.printf("Faster BFS time:\n");
  let (fbfs_path, bfs_expanded) =
    Absbook.call_reporting_time FastBFSG.solve () in
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves bfs_path));
  assert (Puzzle.is_goal (Puzzle.execute_moves fbfs_path));
  assert (List.length fbfs_path = List.length bfs_path);

  Printf.printf("DFS time:\n");
  let (dfs_path, dfs_expanded) =
    Absbook.call_reporting_time DFSG.solve () in
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves dfs_path));

  Printf.printf("DONE TESTING RANDOMLY GENERATED 2x2 TILE PUZZLE\n");

  BFSG.draw bfs_expanded bfs_path ;;

let _ = test_tile_puzzle() ;;

(*......................................................................
                       SAMPLE TILE PUZZLE TESTING
*)

let _  = Random.init 0 ;;

let cDIMS = 3, 3 ;;
let solved : board =
  [| [|Tile 1; Tile 2; Tile 3|];
     [|Tile 4; Tile 5; Tile 6|];
     [|Tile 7; Tile 8; EmptyTile|]; |] ;;

let rand_elt l : board =
  fst (List.nth l (Random.int (List.length l))) ;;

let random_tileboard () : board =
  let cINITIAL_MOVE_COUNT = 45 in
  let module Puzzle : (PUZZLEDESCRIPTION with type state = Tiles.board
                                          and type move = Tiles.direction) =
    MakeTilePuzzleDescription (struct
      let initial = solved
      let dims = cDIMS
    end) in
  let rec make_moves n b =
    if n <= 0 then b
    else make_moves (n - 1) (rand_elt (Puzzle.neighbors b)) in
  make_moves cINITIAL_MOVE_COUNT Puzzle.initial_state ;;

let test_tile_puzzle () : unit =

  let module Puzzle : (PUZZLEDESCRIPTION with type state = Tiles.board
                                          and type move = Tiles.direction) =
    MakeTilePuzzleDescription
      (struct
        let initial = random_tileboard ()
        let dims = cDIMS
      end) in

  Printf.printf("TESTING RANDOMLY GENERATING 3x3 TILEPUZZLE...\n");
  assert (not (Puzzle.is_goal Puzzle.initial_state));

  let module DFSG = DFSSolver(Puzzle) in
  let module BFSG = BFSSolver(Puzzle) in
  let module FastBFSG = FastBFSSolver(Puzzle) in

  Printf.printf("Regular BFS time:\n");
  let (bfs_path, _bfs_expanded) =
    Absbook.call_reporting_time BFSG.solve () in
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves bfs_path));

  Printf.printf("Faster BFS time:\n");
  let (fbfs_path, bfs_expanded) =
    Absbook.call_reporting_time FastBFSG.solve () in
  flush stdout;
  assert (Puzzle.is_goal (Puzzle.execute_moves bfs_path));
  assert (Puzzle.is_goal (Puzzle.execute_moves fbfs_path));
  assert (List.length fbfs_path = List.length bfs_path);

  (* Printf.printf("DFS time:\n");
     let (dfs_path, dfs_expanded) =
     Absbook.call_reporting_time DFSG.solve () in
     flush stdout;
     assert (Puzzle.is_goal (Puzzle.execute_moves dfs_path)); *)

  Printf.printf("DONE TESTING RANDOMLY GENERATED 3x3 TILE PUZZLE\n");

  BFSG.draw bfs_expanded bfs_path ;;

let _ = test_tile_puzzle() ;;

(*......................................................................
                       SAMPLE MAZE PUZZLE TESTING
*)

let init_maze = [|
  [| EmptySpace; EmptySpace; Wall; EmptySpace; EmptySpace|];
  [| Wall; EmptySpace; EmptySpace; EmptySpace; EmptySpace|];
  [| Wall; Wall; EmptySpace; Wall; EmptySpace|];
  [| EmptySpace; EmptySpace; EmptySpace; Wall; EmptySpace|];
  [| EmptySpace; Wall; EmptySpace; EmptySpace; EmptySpace|];
|] ;;

let impossible_maze = [|
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace|];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace|];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace|];
  [| EmptySpace; EmptySpace; EmptySpace; Wall; Wall|];
  [| EmptySpace; EmptySpace; EmptySpace; Wall; EmptySpace|];
|] ;;

let bigger_impossible_maze = [|
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
     EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
  |];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
     EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
  |];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
     EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
  |];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
     EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
  |];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
     EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
  |];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
     EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
  |];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
     EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
  |];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
     EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
  |];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
     EmptySpace; EmptySpace; EmptySpace; Wall; Wall;
  |];
  [| EmptySpace; EmptySpace; EmptySpace; EmptySpace; EmptySpace;
     EmptySpace; EmptySpace; EmptySpace; Wall; EmptySpace;
  |];
|] ;;

let square_maze (copies : int) : maze =
  let orig = 5 in
  let new_maze = Array.make_matrix
      (orig * copies) (orig * copies)
      EmptySpace in
  let col_bound = (orig * copies) in
  let row_bound = (orig * copies) - orig in

  let rec copy_maze (crow: int) (ccol: int) : maze =
    if (ccol = col_bound && crow = row_bound) then new_maze
    else if (ccol = col_bound) then
      copy_maze (crow + orig) 0
    else
      begin
        List.init orig Fun.id
        |> List.iter (fun offset ->
            Array.blit init_maze.((crow + offset) mod orig) 0
              new_maze.(crow + offset) ccol orig);
        copy_maze (crow) (ccol + orig)
      end in

  copy_maze 0 0 ;;

module TestMazeI : MAZEINFO =
struct
  let maze = square_maze 1
  let initial_pos =  (0,0)
  let goal_pos = (4,4)
  let dims = (5, 5)
end

module TestMazeII : MAZEINFO =
struct
  let maze = square_maze 2
  let initial_pos =  (0, 0)
  let goal_pos = (9, 9)
  let dims = (10, 10)
end

module TestMazeIII : MAZEINFO =
struct
  let maze = square_maze 3
  let initial_pos = (0, 0)
  let goal_pos = (14, 14)
  let dims = (15, 15)
end

module TestMazeIV : MAZEINFO =
struct
  let maze = square_maze 4
  let initial_pos = (0, 0)
  let goal_pos = (14, 14)
  let dims = (15, 15)
end

module TestMazeV : MAZEINFO =
struct
  let maze = impossible_maze
  let initial_pos = (0, 0)
  let goal_pos = (4, 4)
  let dims = (5, 5)
end

module TestMazeVI : MAZEINFO =
struct
  let maze = bigger_impossible_maze
  let initial_pos = (0, 0)
  let goal_pos = (9, 9)
  let dims = (10, 10)
end

module TestMazePuzzle (M : MAZEINFO) =
struct
  module MPuzzle = MakeMazePuzzleDescription(M)
  module DFSG = DFSSolver (MPuzzle)
  module FastBFSG = FastBFSSolver (MPuzzle)
  module BFSG = BFSSolver (MPuzzle)
  let run_tests () =
    Printf.printf("TESTING MAZE PUZZLE...\n");

    Printf.printf("Regular BFS time:\n");
    let (bfs_path, _bfs_expanded) =
      Absbook.call_reporting_time BFSG.solve () in
    assert (MPuzzle.is_goal (MPuzzle.execute_moves bfs_path));

    Printf.printf("Fast BFS time:\n");
    let (fbfs_path, bfs_expanded) =
      Absbook.call_reporting_time FastBFSG.solve () in
    assert (MPuzzle.is_goal (MPuzzle.execute_moves fbfs_path));

    assert ((List.length fbfs_path) = (List.length bfs_path));

    Printf.printf("DFS time:\n");
    let (dfs_path, dfs_expanded) =
      Absbook.call_reporting_time DFSG.solve () in
    assert (MPuzzle.is_goal (MPuzzle.execute_moves dfs_path));

    Printf.printf("DONE TESTING MAZE PUZZLE, DISPLAYING MAZE NOW\n");
    BFSG.draw bfs_expanded bfs_path;
    DFSG.draw dfs_expanded dfs_path

  let run_tests_error () =
    Printf.printf("TESTING IMPOSSIBLE MAZE PUZZLE...\n");

    Printf.printf("Regular BFS time (msecs):\n");
    let before = Sys.time () in
    let _ = (try let _ = BFSG.solve () in ()
             with
             | BFSG.CantReachGoal ->
               Printf.printf ("%.*f\n") 6 ((Sys.time() -. before) *. 1000.);
             | _ ->  ()) in

    Printf.printf("Fast BFS time (msecs):\n");
    let before1 = Sys.time () in
    let _ = (try let _ = FastBFSG.solve () in ()
             with
             | FastBFSG.CantReachGoal ->
               Printf.printf ("%.*f\n") 6 ((Sys.time() -. before1) *. 1000.);
             | _ ->  ()) in

    Printf.printf("DFS time (msecs):\n");
    let before2 = Sys.time () in
    try let _ = DFSG.solve () in ()
    with
    | DFSG.CantReachGoal ->
      Printf.printf ("%.*f\n") 6 ((Sys.time() -. before2) *. 1000.);
    | _ ->  ();

end ;;

module MI   = TestMazePuzzle (TestMazeI)
module MII  = TestMazePuzzle (TestMazeII)
module MIII = TestMazePuzzle (TestMazeIII)
module MIV = TestMazePuzzle (TestMazeIV)
module MV = TestMazePuzzle (TestMazeV)
module MVI = TestMazePuzzle (TestMazeVI)

let _ =
  MI.run_tests ();
  MII.run_tests ();
  MIII.run_tests ();
  MIV.run_tests ();
  MV.run_tests_error ();
  MVI.run_tests_error ();
  
