open Types

type msg = 
  Click of int * int
         
type model = {
    current_board: int
  ; pooter_thinking: bool
  ; turn: point 
  ; game: board list
  ; game_winner: winner
  }

 
