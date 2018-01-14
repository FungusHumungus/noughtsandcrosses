open Tea
open Tea.App
open Tea.Html
   
open Types
open! Board


module MoreList = struct
  let rec init count initfn =
    if count == 0 
    then []
    else (initfn ()) :: init (count - 1) initfn
end
           
let new_board () = 
  { board = MoreList.init 9 (fun _ -> Empty)
  ; winner = None }
   
let init () = 
  let empty_game = MoreList.init 9 (fun _ -> new_board ()) in
  ({
    current_board = 4
  ; turn = Nought
  ; game = empty_game
  ; game_winner = None
  }, Cmd.none)
            
let update model = function
  | Click (board, point) -> 
     if model.game_winner == None
     then
       let (valid, game) = Board.make_move model.game model.current_board model.turn board point in
       if valid
       then 
         match get_overall_winner game with
         | None ->
            if Board.full_board (List.nth model.game point).board
            then ({ model with game_winner = Draw}, Cmd.none)
            else let turn = change_turn model.turn in
                 ({ model with game = game
                             ; current_board = point
                             ; turn = turn
                  }, if turn == Cross 
                     then Agent.move game point turn
                     else Cmd.none)
         | _ as winner -> ({ model with game = game
                                     ; game_winner = winner },
                          Cmd.none)
                       
       else (model, Cmd.none)
     else (model, Cmd.none)
       
let border_style top right bottom left =
  (string_of_int top) ^ "px " ^
  (string_of_int right) ^ "px " ^
  (string_of_int bottom) ^ "px " ^
  (string_of_int left) ^ "px"
            
let border_for_idx = function
  | 0 -> border_style 0 1 1 0
  | 1 -> border_style 0 1 1 1
  | 2 -> border_style 0 0 1 1
  | 3 -> border_style 1 1 1 0
  | 4 -> border_style 1 1 1 1
  | 5 -> border_style 1 0 1 1
  | 6 -> border_style 1 1 0 0 
  | 7 -> border_style 1 1 0 1
  | 8 -> border_style 1 0 0 1
  | _ -> ""
     
  
let point_view board_idx idx point = 
  div [ class' "point"
      ; style "border-width" (border_for_idx idx)
      ; onClick @@ Click (board_idx, idx)]
      [ text begin match point with 
             | Empty -> " "
             | Nought -> "O"
             | Cross -> "X"
             end
      ]

let board_view model idx { board = board
                         ; winner = winner } = 
  div [ class' "board"
      ; style "background" begin match winner with
                           | Noughts -> "#c9e7db"
                           | Crosses ->  "#eb7777"
                           | _ -> "white"
                           end
      ; style "box-shadow" @@ if model.current_board == idx
                                then "0px 0px 10px rgb(117, 110, 234)"
                                else "none"
      ]
  @@ List.mapi (point_view idx) board
  
  
let view model = 
  div []
      [ h1 [] [ text "Noughts and Crosses"
              ; span [ class' "extreme"]
                     [ text "Extreme" ] ]
      ; div [ style "display" @@ if model.game_winner == None
                                 then "none"
                                 else "block" ]
            [ text @@ "Winner : " ^ match model.game_winner with
                                              | None -> ""
                                              | Noughts -> "Noughts"
                                              | Crosses -> "Crosses"
                                              | Draw -> "Draw"
            ]
      ; div [ class' "game" ] 
        @@ List.mapi (board_view model) model.game
      ]
   
let subscriptions _ = Sub.none

let main = 
  standardProgram {
     init
    ; update
    ; view
    ; subscriptions
    }
