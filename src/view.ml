open Tea.Html
open Model
open! Types
open! Board
       
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
                           | Nought -> "#c9e7db"
                           | Cross ->  "#eb7777"
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
      ; div [ class' "thinking" 
            ; style "visibility" @@ if model.pooter_thinking then "visible" else "hidden" ]
            [ text "thinking..." ]
      ; div [ class' "game" ] 
        @@ List.mapi (board_view model) model.game
      ]
