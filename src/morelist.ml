(** Add some missing functions to List *)
module List = struct
  include List
  let rec init count initfn =
    if count == 0 
    then []
    else (initfn ()) :: init (count - 1) initfn
    
  let empty = function
    | [] -> true
    | _ -> false
    
        
  let rec range from to' = 
    if from == to' 
    then []
    else from :: range (from + 1) to'
    
end
 
