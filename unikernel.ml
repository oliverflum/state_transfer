open Lwt.Infix
module StringMap = Map.Make(String)

module Main (TIME: Mirage_time.S) (PClock: Mirage_clock.PCLOCK) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) (R: Mirage_random.S) = struct

  module S = Store.Make (TIME) (PClock)

  type adjacency_rec = {
    atomic_function: string;
    condition: bool;
  };;

  let f1 store = 
    Logs.info (fun m -> m "F1");
    let rand = (Randomconv.int ~bound:10 R.generate) in
    Logs.info (fun m -> m "Created random number %i" rand);
    store#set "test" (S.VInt rand);
    Lwt.return ()

  let f2 store = 
    Logs.info (fun m -> m "F2");
    TIME.sleep_ns (Duration.of_sec 2) >>= fun () ->
    Lwt.return ()

  let f3 store = 
    Logs.info (fun m -> m "F3");
    TIME.sleep_ns (Duration.of_sec 3) >>= fun () ->
    Lwt.return ()

  let get_adjacency store =
    let assoc_adj_list = [
      ("f1", [
        {atomic_function = "f2"; condition = ((S.to_int (store#get "test" (S.VInt 0))) >= 5)};
        {atomic_function = "f3"; condition = ((S.to_int (store#get "test" (S.VInt 0))) < 5)};
      ]);
      ("f2", [{atomic_function = "f1"; condition = true}]);
      ("f3", [{atomic_function = "f1"; condition = true}]);
    ] in
    StringMap.of_seq (List.to_seq assoc_adj_list)

  let terminate store = 
    store#terminate

  let rec get_next_function_name = function
    | [] -> "terminate"
    | hd::tail -> if hd.condition == true then hd.atomic_function else get_next_function_name tail

  let next_function functions fnext_name =
    match fnext_name with
      | "terminate" -> terminate
      | _ -> StringMap.find fnext_name functions

  let rec run functions store curr pclock= 
    S.check_control_status >>= fun status ->
      Logs.info (fun m -> m "Read control message %s" (S.type_of_action status));
      match status with
        | S.Resume -> begin
            let fnext = next_function functions curr in
            fnext store >>= fun () ->
            let adjacency = get_adjacency store in
            let adj_arr = StringMap.find curr adjacency in
            let fnext_name = get_next_function_name adj_arr in
            store#set "next" (S.VString fnext_name);
            run functions store fnext_name pclock
          end
        | _ -> store#suspend pclock status

  let start _time pclock res (ctx: CON.t) _r =
    let tstr = S.time pclock in
    Logs.info (fun m -> m "start-TS: %s" tstr);
    let token = Key_gen.token () in
    let repo = Key_gen.repo () in
    let migration = Key_gen.migration () in
    let id = Key_gen.id () in
    let host_id = Key_gen.hostid () in
    let store = new S.webStore ctx res repo token id host_id in
    let functions = StringMap.of_seq (List.to_seq [("f1", f1); ("f2", f2); ("f3", f3)]) in
    store#init migration pclock >>= fun _ ->
    let last_fct = (S.to_str (store#get "next" (S.VString "f1"))) in
    run functions store last_fct pclock
end
