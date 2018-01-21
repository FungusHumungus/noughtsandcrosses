open Morelist

module Array = struct
  include Array
  let for_all fn arr = 
    Array.fold_left (fun b element -> if b 
                                   then (fn element)
                                   else b) true arr
        
end
             
type mutable_board = { mboard : Board.point array
                     ; mwinner : Board.point }
                   
type mutable_game = mutable_board array

module MutableBoard : Board.AtPoint 
     with type t = Board.point array = struct
                 type t = Board.point array
                 let value_at_point board pos = 
                   Array.get board pos
               end
                   
module MutableGame : Board.AtPoint 
       with type t = mutable_game = struct
                   type t = mutable_game
                   let value_at_point board pos = 
                     let b = Array.get board pos in
                     b.mwinner
                 end
     
let game_to_mutable_game (game: Board.board list) = 
  let board_to_mutable_board board = Board.{ mwinner = board.winner
                                           ; mboard = Array.of_list board.board } in 
  Array.of_list @@ List.map board_to_mutable_board game
  
let rec random_move ~game ~board =
  let point = (Random.int 9) in
  let board' = (Array.get game board).mboard in
  let module Rules = Board.Rules(MutableBoard) in
  if Rules.is_valid_move board' point
  then point
  else random_move ~game ~board

let full_board board = 
  Array.for_all (fun item -> item != Board.Empty) board
 
let rec play_game (game: mutable_game) (current_board: int) move turn = 

  let board = Array.get game current_board in
  let board' = board.mboard in
  board'.(move) <- turn;
  if board.mwinner == Board.Empty
  then 
    let module Check = Board.CheckWinner(MutableBoard) in
    let winner = Check.winner board' in
    game.(current_board) <- { mboard = board'
                           ; mwinner = winner };
  else
    ();
  let module Check = Board.CheckWinner(MutableGame) in
  let winner = Check.winner game in
  match winner with
  | Empty -> 
     let module Rules = Board.Rules(MutableBoard) in
     if Rules.valid_moves (game.(move).mboard) |> List.empty
     then Board.Draw
     else play_game game move (random_move ~game ~board:move) (Board.change_turn turn)
  | Nought -> Board.Noughts
  | Cross -> Board.Crosses
           
let did_i_win turn winner = 
  let open Board in
  match turn, winner with
  | Nought, Noughts -> true
  | Cross, Crosses -> true
  | _ -> false
      
      
let string_of_wins wins = 
  let concat str (move, count) = str ^ "(" ^ (string_of_int move)
                                 ^ ", " ^ (string_of_int count) ^ ")" in
  List.fold_left concat "" wins
  
  
(** The number of random games to play per move. *)
let rollouts = 1000
  
let move game current_board whos_turn = 
  let rec get_wins count wins move = 
    if count = rollouts
    then (move, wins)
    else let mgame = game_to_mutable_game game in
         let win = play_game mgame current_board move whos_turn in
         get_wins (count + 1) (if did_i_win whos_turn win then wins + 1 else wins) move
    
  in
  let module Rules = Board.Rules(Board.Board) in 
  let wins = Rules.valid_moves (List.nth game current_board).board
             |> List.map (get_wins 0 0) in
  let compare_moves (_, wins) (_, wins') = compare wins' wins in
  Js.log @@ string_of_wins wins;
  List.sort compare_moves wins
  |> List.hd
  |> fst
  
  
