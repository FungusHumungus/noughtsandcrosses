
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
           
 
