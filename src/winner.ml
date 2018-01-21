open Types           

module type AtPoint = sig
  type t
  val value_at_point : t -> int -> point
end
  
(**  Get the position of a board *)
module BoardPoint : AtPoint 
       with type t = point list = struct
                   type t = point list
                   let value_at_point board pos = 
                     List.nth board pos
                 end
     
(** Get the winner of a particular board *)
module GamePoint : AtPoint 
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
