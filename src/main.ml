open Tea
open Tea.App
   
open Types
open! Board

open Morelist

          
let new_board () = 
  { board = List.init 9 (fun _ -> Empty)
  ; winner = Empty }
   
let init () = 
  let empty_game = List.init 9 (fun _ -> new_board ()) in
  ({ pooter_thinking = false
   ; current_board = 4
   ; turn = Nought
   ; game = empty_game
   ; game_winner = None
   }, Cmd.none)
            
let update model = function
  | Click (board, point) -> 
     if model.game_winner == None
     then
       let (valid, game) = make_move model.game model.current_board model.turn board point in
       if valid
       then 
         let module Check = CheckWinner(Game) in
         let module Rules = Rules(Board) in
         match Check.winner game with
         | Empty ->
            if Rules.valid_moves (List.nth model.game point).board 
               |> List.empty
            then ({ model with game_winner = Draw}, Cmd.none)
            else let turn = change_turn model.turn in
                 let pooters_turn = turn == Cross in
                 ({ model with pooter_thinking = pooters_turn
                             ; game = game
                             ; current_board = point
                             ; turn = turn
                  }, if pooters_turn
                     then Agent.move game point turn
                     else Cmd.none)
         | Nought -> ({ model with game = game
                                ; pooter_thinking = false
                                ; game_winner = Noughts },
                     Cmd.none)
         | Cross -> ({ model with game = game
                               ; pooter_thinking = false
                               ; game_winner = Crosses },
                    Cmd.none)
                       
       else (model, Cmd.none)
     else (model, Cmd.none)
   
let subscriptions _ = Sub.none

let main = 
  standardProgram {
      init
    ; update
    ; view = View.view
    ; subscriptions
    }
