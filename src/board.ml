open Morelist
   
type winner = 
  | None
  | Noughts
  | Crosses
  | Draw
  
type point = 
  | Empty
  | Nought
  | Cross
  
type board = { board : point list  
             ; winner : point }
           
           
module type AtPoint = sig
  type t
  val value_at_point : t -> int -> point

end
                    
module Board : AtPoint 
       with type t = point list = struct
                   type t = point list
                   let value_at_point board pos = 
                     List.nth board pos
                 end
     
module Game : AtPoint 
       with type t = board list = struct
                   type t = board list
                   let value_at_point board pos = 
                     let b = List.nth board pos in
                     b.winner
                 end

module CheckWinner (Point: AtPoint) = struct
  type t = Point.t
  let turn_at_points board points = 
    let turn_at_point point = Point.value_at_point board point in
    let turns = List.map turn_at_point points in
    let head = List.hd turns in
    if head == Empty 
    then Empty
    else begin if List.for_all (fun item -> item = head) turns
               then head
               else Empty
         end
    
  let winning_combinations = 
    [ [0;1;2]; [3;4;5]; [6;7;8]
      ; [0;3;6]; [1;4;7]; [2;5;8]
      ; [0;4;8]; [2;4;6] ] 

  let winner board = 
    let lines = List.map (turn_at_points board) winning_combinations in
    let winning_lines = List.filter (fun line -> line != Empty) lines in
    if List.length winning_lines > 0 
    then List.hd winning_lines
    else Empty
end
  
module Rules (Point: AtPoint) = struct
 
  let is_valid_move board point = Point.value_at_point board point == Empty

  let valid_moves board = 
    List.range 0 8 |>
      List.filter (is_valid_move board)
  
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
      
 
let make_move game current_board turn board point = 
  let update_point idx point' = if idx == point 
                                then if point' == Empty 
                                     then turn
                                     else point'
                                else point'
  in
  let 
    update_board idx board' = if idx == board
                              then let new_board = List.mapi update_point board'.board in
                                   { board = new_board
                                   ; winner = let module Check = CheckWinner(Board) in
                                              if board'.winner == Empty
                                              then Check.winner new_board
                                              else board'.winner
                                   }
                              else board'
  in
  let valid_move = (turn_at_board_point game board point) == Empty in
  if (current_board == board) && valid_move
  then let game' = List.mapi update_board game in
       (true, game') 
  else (false, game)
 
