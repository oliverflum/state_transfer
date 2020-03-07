open Lwt.Infix

module Make (S: Store.Make)= struct
  
  class StateMachine adjecency functions store xs_client =
    let adjacencey_map = StringMap.of_seq (List.to_seq adjacency)
    let functions_map = StringMap.of_seq (List.to_seq functions)
    object (self)
      val adjacency = adjacencey_map
      val functions = functions_map
      val store = store
      val xs_client = xsclient

      let terminate = 
        store#terminate

      method eval = function
        | (string s1, string s2, ('a -> 'b -> bool) op) -> begin
            let s1v = (S.to_str (store#get s1 (S.VString ""))) in
            let s2v = (S.to_str (store#get s2 (S.VString ""))) in
            op s1v s2v
          end
        | _ -> false 

      method get_next_function_name curr =
        let adj_arr = StringMap.find curr adjacency in
        let rec inner = function
            | [] -> "terminate"
            | hd::tail -> if (eval hd.condition) then hd.atomic_function else get_next_function_name tail
        in 
        inner adj_arr

      method get_function name =
        match name with
          | "terminate" -> terminate
          | _ -> StringMap.find name functions

      method run curr xs_client pclock =
        S.read_shutdown_value xs_client >>= fun status ->
        Logs.info (fun m -> m "Read control message %s" (S.type_of_action status));
        match status with
          | S.Resume -> begin
              let f = self#get_function curr in
              f store >>= fun () ->
              let fnext_name = self#get_next_function_name curr in
              store#set "next" (S.VString fnext_name);
              run functions store fnext_name xs_client pclock
            end
          | _ -> store#suspend pclock status
    end 
end