open Model
   
external set_timeout : (unit -> unit) -> int -> unit = "setTimeout" [@@bs.val]

let move game current_board whos_turn = 
  Tea_cmd.call ( fun callbacks ->
                 let enqRes result = !callbacks.enqueue ( Click (current_board, result) ) in
                 
                 set_timeout (fun () ->
                     MctsAgent.move game current_board whos_turn
                            |> enqRes
                            |> ignore) 100
               )
