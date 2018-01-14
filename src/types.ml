open Board

type msg = 
  Click of int * int
   
type model = {
    current_board: int
  ; turn: Board.point
  ; game: board list
  ; game_winner: Board.winner
  }
 
