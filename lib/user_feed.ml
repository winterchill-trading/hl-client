open! Core
open Async
open Hl_protocol

module Update = struct
  type t =
    | Fills of
        { user : Address.t
        ; is_snapshot : bool
        ; fills : User_fill.t list
        }
    | Position of
        { user : Address.t
        ; coin : Coin.t
        ; szi : Size.t
        ; time : int
        }
    | Resync of { user : Address.t }
  [@@deriving sexp_of]
end

module User_state = struct
  type t =
    { seen_tids : Int.Hash_set.t
    ; positions : (Size.t * int) Coin.Table.t (** szi and fill time *)
    ; mutable consumers : Update.t Pipe.Writer.t list
    }

  let create () =
    { seen_tids = Int.Hash_set.create ()
    ; positions = Coin.Table.create ()
    ; consumers = []
    }
  ;;
end

type t =
  { ws : Ws.t
  ; max_users : int
  ; users : User_state.t Address.Table.t
  }

let default_max_users = 10

let publish (state : User_state.t) update =
  state.consumers <- List.filter state.consumers ~f:(fun w -> not (Pipe.is_closed w));
  List.iter state.consumers ~f:(fun w -> Pipe.write_without_pushback w update)
;;

let position_after (fill : User_fill.t) =
  match fill.side with
  | Buy -> Size.(fill.start_position + fill.sz)
  | Sell -> Size.(fill.start_position - fill.sz)
;;

let on_fills t ~user ~is_snapshot fills =
  match Hashtbl.find t.users user with
  | None -> ()
  | Some state ->
    let fresh =
      List.filter fills ~f:(fun (fill : User_fill.t) ->
        if Hash_set.mem state.seen_tids fill.tid
        then false
        else (
          Hash_set.add state.seen_tids fill.tid;
          true))
    in
    if not (List.is_empty fresh)
    then (
      publish state (Fills { user; is_snapshot; fills = fresh });
      (* Latest fill per coin; fills within a message are not guaranteed to be ordered, so
         compare times (then trade ids). *)
      let latest = Coin.Table.create () in
      List.iter fresh ~f:(fun (fill : User_fill.t) ->
        Hashtbl.update latest fill.coin ~f:(function
          | Some (prev : User_fill.t)
            when prev.time > fill.time || (prev.time = fill.time && prev.tid > fill.tid)
            -> prev
          | _ -> fill));
      Hashtbl.iteri latest ~f:(fun ~key:coin ~data:fill ->
        let newer =
          match Hashtbl.find state.positions coin with
          | Some (_, time) -> fill.time >= time
          | None -> true
        in
        if newer
        then (
          let szi = position_after fill in
          Hashtbl.set state.positions ~key:coin ~data:(szi, fill.time);
          publish state (Position { user; coin; szi; time = fill.time }))))
;;

let on_reconnect t () =
  Hashtbl.iteri t.users ~f:(fun ~key:user ~data:state -> publish state (Resync { user }))
;;

let create
  ?(max_users = default_max_users)
  ?ping_interval
  ?initial_backoff
  ?max_backoff
  config
  =
  let t_ref = ref None in
  let on_reconnect () = Option.iter !t_ref ~f:(fun t -> on_reconnect t ()) in
  match%map
    Ws.connect ?ping_interval ?initial_backoff ?max_backoff ~on_reconnect config
  with
  | Error _ as error -> error
  | Ok ws ->
    let t = { ws; max_users; users = Address.Table.create () } in
    t_ref := Some t;
    don't_wait_for
      (Pipe.iter_without_pushback (Ws.messages ws) ~f:(function
        | User_fills { is_snapshot; user; fills } -> on_fills t ~user ~is_snapshot fills
        | _ -> ()));
    Ok t
;;

let users t = Hashtbl.keys t.users |> List.sort ~compare:Address.compare

let consumers t ~address =
  match Hashtbl.find t.users address with
  | None -> 0
  | Some state -> List.count state.consumers ~f:(fun w -> not (Pipe.is_closed w))
;;

let release t ~address writer =
  match Hashtbl.find t.users address with
  | None -> return ()
  | Some state ->
    state.consumers
    <- List.filter state.consumers ~f:(fun w ->
         (not (phys_equal w writer)) && not (Pipe.is_closed w));
    if List.is_empty state.consumers
    then (
      Hashtbl.remove t.users address;
      Ws.unsubscribe t.ws (User_fills { user = address }))
    else return ()
;;

let subscribe t ~address =
  let add_consumer (state : User_state.t) =
    let reader, writer = Pipe.create () in
    (* A late consumer starts from the positions known so far. *)
    Hashtbl.iteri state.positions ~f:(fun ~key:coin ~data:(szi, time) ->
      Pipe.write_without_pushback
        writer
        (Update.Position { user = address; coin; szi; time }));
    state.consumers <- writer :: state.consumers;
    upon (Pipe.closed writer) (fun () -> don't_wait_for (release t ~address writer));
    reader
  in
  match Hashtbl.find t.users address with
  | Some state -> return (Ok (add_consumer state))
  | None ->
    if Hashtbl.length t.users >= t.max_users
    then
      return
        (Or_error.error_s
           [%message
             "too many user addresses on one connection"
               ~max_users:(t.max_users : int)
               (address : Address.t)])
    else (
      let state = User_state.create () in
      Hashtbl.set t.users ~key:address ~data:state;
      let reader = add_consumer state in
      let%map () = Ws.subscribe t.ws (User_fills { user = address }) in
      Ok reader)
;;

let close t =
  Hashtbl.iter t.users ~f:(fun state ->
    List.iter state.consumers ~f:Pipe.close;
    state.consumers <- []);
  Hashtbl.clear t.users;
  Ws.close t.ws
;;
