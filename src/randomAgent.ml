open Board
   
let rec move game current_board whos_turn = 
  let point = (Random.int 9) in
  if turn_at_board_point game current_board point == Empty
  then point
  else move game current_board whos_turn
