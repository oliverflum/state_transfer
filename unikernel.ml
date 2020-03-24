open Lwt.Infix
module StringMap = Map.Make(String)
module S = Store

module Main (TIME: Mirage_time.S) (PClock: Mirage_clock.PCLOCK) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) (R: Mirage_random.S) = struct

  module C = Control.Make (TIME) (PClock)

  (*Data type for adjacency Matrix*)
  type adjacency_rec = {
    atomic_function: string;
    condition: bool;
  }

  (*Custom defined functions*)
  let f1 store =
    Logs.info (fun m -> m "F1");
    let round = (S.to_int (store#get "round" (S.VInt 0))) in
    let rand = (Randomconv.int ~bound:10 R.generate) in
    Logs.info (fun m -> m "Created random number %i" rand);
    TIME.sleep_ns (Duration.of_sec 2) >>= fun () ->
    store#set "test" (S.VInt rand);
    store#set "round" (S.VInt (round + 1));
    Lwt.return ()

  let f2 store =
    Logs.info (fun m -> m "F2");
    TIME.sleep_ns (Duration.of_sec 2) >>= fun () ->
    Lwt.return ()

  let f3 store =
    Logs.info (fun m -> m "F3");
    TIME.sleep_ns (Duration.of_sec 3) >>= fun () ->
    Lwt.return ()

  (*The function passed on termination*)
  let terminate store = 
    store#terminate
  
  (*Define the adjacency matrix here as a associative list*)
  let get_adjacency store =
    let f12 = (((S.to_int (store#get "test" (S.VInt 0))) >= 5) && ((S.to_int (store#get "round" (S.VInt 0))) <=10)) in
    let f13 = (((S.to_int (store#get "test" (S.VInt 0))) < 5) && ((S.to_int (store#get "round" (S.VInt 0))) <=10)) in
    let assoc_adj_list = [
      ("f1", [
        {atomic_function = "f2"; condition = f12};
        {atomic_function = "f3"; condition = f13};
        {atomic_function = "terminate"; condition = ((S.to_int (store#get "round" (S.VInt 0))) > 10)};
      ]);
      ("f2", [{atomic_function = "f1"; condition = true}]);
      ("f3", [{atomic_function = "f1"; condition = true}]);
    ] in
    let amap = StringMap.of_seq (List.to_seq assoc_adj_list) in
    amap

  (*Define a mapping from function names to functions. Required for serialisation*)
  let get_function_map =
    let functions = [("f1", f1); ("f2", f2); ("f3", f3)] in
    let fmap = StringMap.of_seq (List.to_seq functions) in
    fmap

  (*These functions do not need to be edited*)
  let get_next_function_name store (curr: string) = 
    let adjacency = get_adjacency store in
    let adj_arr = StringMap.find curr adjacency in
    let rec inner = function
      | [] -> "terminate"
      | hd::tail -> if hd.condition == true then hd.atomic_function else inner tail
    in inner adj_arr

  let get_function store name =
    let functions = get_function_map in
    match name with
      | "terminate" -> terminate
      | _ -> StringMap.find name functions

  let rec run store (client: OS.Xs.client) (curr: string) =
    C.read_shutdown_value client >>= fun status ->
    Logs.info (fun m -> m "Read control message %s" (Control.Status.string_of_status status));
    match status with
      | Control.Status.Resume -> begin
          let f = get_function store curr in
          f store >>= fun () ->
          let fnext_name = get_next_function_name store curr in
          store#set "next" (S.VString fnext_name);
          run store client fnext_name 
        end
      | _ -> begin 
          let time = C.time in 
          store#suspend time status
        end

  let start _time _pclock resolver conduit _random =
    let tstr = C.time in
    Logs.info (fun m -> m "start-TS: %s" tstr);
    let token = Key_gen.token () in
    let repo = Key_gen.repo () in
    let migration = Key_gen.migration () in
    let id = Key_gen.id () in
    let host_id = Key_gen.hostid () in
    let store = new S.webStore conduit resolver repo token id host_id in
    let time = C.time in 
    store#init time migration (C.steady) >>= fun _ ->
    let fct = (S.to_str (store#get "next" (S.VString "f1"))) in
    OS.Xs.make () >>= fun client ->
    run store client fct
end
