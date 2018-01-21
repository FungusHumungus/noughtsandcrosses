open Morelist
open! Types
open Winner
   
module type Game = sig
  type t
  val value_at_point : t -> int -> int -> point
end
  
module Rules (Game: Game)
             (Point: AtPoint with type t = Game.t) = struct
 
  module Winner = CheckWinner(Point)
                
  let is_valid_move game board point = Game.value_at_point game board point == Empty

  let valid_moves game board = 
    List.range 0 8 |>
      List.filter (is_valid_move game board)
  
  let result game current_board =
    match Winner.winner game with
    | Empty ->
       if valid_moves game current_board |> List.empty
        then Draw
       else None
    | Nought -> Noughts
    | Cross -> Crosses
   
end
                          
                          
module PersistentGame : Game
     with type t = board list = struct
                 type t = board list
                 let value_at_point game board pos = 
                   let board' = (List.nth game board).board in
                   List.nth board' pos
               end
     
 
let turn_at_board_point game board point = 
  let board' = List.nth game board in
  List.nth board'.board point
  
 
let full_board board = 
  List.for_all (fun item -> item != Empty) board
 
let change_turn = function
  | Nought -> Cross
  | Cross -> Nought
  | _ -> failwith "Dont be silly"
      
(** Make a move on our immutable board. *)
let make_move (game: board list) current_board turn board point = 
  let update_point idx point' = if idx == point 
                                then turn
                                else point'
  in
  let 
    update_board idx board' = if idx == board
                              then let new_board = List.mapi update_point board'.board in
                                   { board = new_board
                                   ; winner = let module Check = CheckWinner(BoardPoint) in
                                              if board'.winner == Empty
                                              then Check.winner new_board
                                              else board'.winner
                                   }
                              else board'
  in
  let module Rules = Rules(PersistentGame)(Winner.GamePoint) in
  let valid_move = Rules.is_valid_move game board point in
  if (current_board == board) && valid_move
  then let game' = List.mapi update_board game in
       (true, game') 
  else (false, game)
 
