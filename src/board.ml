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
             ; winner : winner }
 
let turn_at_board_point game board point = 
  let board' = List.nth game board in
  List.nth board'.board point
  
let turn_at_points board points = 
  let turn_at_point point = List.nth board point in
  let turns = List.map turn_at_point points in
  let head = List.hd turns in
  if head == Empty 
  then Empty
  else begin if List.for_all (fun item -> item = head) turns
             then head
             else Empty
       end
           
let winner_at_boards game boards = 
  let winner_at_board board = (List.nth game board).winner in
  let winners = List.map winner_at_board boards in
  let head = List.hd winners in
  if head == None
  then None
  else begin if List.for_all (fun item -> item = head) winners
             then head
             else None
       end
  
  
let full_board board = 
  List.for_all (fun item -> item != Empty) board
 
let change_turn = function
  | Nought -> Cross
  | Cross -> Nought
  | _ -> failwith "Dont be silly"
      
let winning_combinations = 
  [ [0;1;2]; [3;4;5]; [6;7;8]
    ; [0;3;6]; [1;4;7]; [2;5;8]
    ; [0;4;8]; [2;4;6] ] 

let get_winner board = 
  let lines = List.map (turn_at_points board) winning_combinations in
  let winning_lines = List.filter (fun line -> line != Empty) lines in
  if List.length winning_lines > 0 
  then match List.hd winning_lines with
       | Nought -> Noughts
       | Cross -> Crosses
       | Empty -> None
  else None
  
let get_overall_winner game = 
  let lines = List.map (winner_at_boards game) winning_combinations in
  let winning_lines = List.filter (fun line -> line != None) lines in
  if List.length winning_lines > 0 
  then List.hd winning_lines
  else None
 
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
                                   ; winner = if board'.winner == None
                                              then get_winner new_board
                                              else board'.winner
                                   }
                              else board'
  in
  let valid_move = (turn_at_board_point game board point) == Empty in
  if (current_board == board) && valid_move
  then let game' = List.mapi update_board game in
       (true, game') 
  else (false, game)
 
