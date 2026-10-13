(* [User_feed] against the loopback websocket server: shared subscriptions, fill
   de-duplication across a reconnect, positions, late consumers and the address limit. *)

open! Core
open Async
open Hl_protocol
open Hl_client
module Server = Loopback_server

let leader = Address.of_string "0x00000000000000000000000000000000000000aa"
let other = Address.of_string "0x00000000000000000000000000000000000000bb"

let fill ~coin ~side ~sz ~start ~time ~tid =
  sprintf
    {|{"coin":"%s","px":"100","sz":"%s","side":"%s","time":%d,"startPosition":"%s","dir":"x","closedPnl":"0","hash":"0x00","oid":1,"crossed":true,"fee":"0","tid":%d}|}
    coin
    sz
    side
    time
    start
    tid
;;

let frame ?(snapshot = false) user fills =
  sprintf
    {|{"channel":"userFills","data":{%s"user":"%s","fills":[%s]}}|}
    (if snapshot then {|"isSnapshot":true,|} else "")
    (Address.to_string user)
    (String.concat ~sep:"," fills)
;;

let f1 = fill ~coin:"BTC" ~side:"B" ~sz:"0.01" ~start:"0" ~time:1 ~tid:1
let f2 = fill ~coin:"ETH" ~side:"A" ~sz:"1" ~start:"0" ~time:2 ~tid:2
let f3 = fill ~coin:"BTC" ~side:"B" ~sz:"0.02" ~start:"0.01" ~time:3 ~tid:3
let f4 = fill ~coin:"BTC" ~side:"A" ~sz:"0.05" ~start:"0.03" ~time:4 ~tid:4

let on_subscribe ~connection (sub : Subscription.t) =
  match sub, connection with
  | User_fills { user }, 1 when Address.equal user leader ->
    [ frame ~snapshot:true leader [ f1; f2 ]; frame leader [ f3 ] ]
  | User_fills { user }, _ when Address.equal user leader ->
    (* After the reconnect the snapshot repeats everything plus one missed fill. *)
    [ frame ~snapshot:true leader [ f1; f2; f3; f4 ] ]
  | User_fills { user }, _ ->
    [ frame
        ~snapshot:true
        user
        [ fill ~coin:"SOL" ~side:"B" ~sz:"3" ~start:"0" ~time:9 ~tid:100 ]
    ]
  | _ -> []
;;

let read_and_print ~label reader n =
  Deferred.repeat_until_finished n (fun n ->
    if n = 0
    then return (`Finished ())
    else (
      match%map Clock.with_timeout (sec 5.) (Pipe.values_available reader) with
      | `Timeout ->
        print_endline (label ^ ": timeout");
        `Finished ()
      | `Result `Eof ->
        print_endline (label ^ ": eof");
        `Finished ()
      | `Result `Ok ->
        (match Pipe.read_now reader with
         | `Ok update -> print_s [%sexp (label : string), (update : User_feed.Update.t)]
         | `Eof | `Nothing_available -> ());
        `Repeat (n - 1)))
;;

let%expect_test "shared leader subscription" =
  Log.Global.set_output [];
  let%bind server = Server.start ~greeting:[] ~on_subscribe () in
  let config = Config.create ~base_url:(Server.base_url server) () in
  let%bind feed =
    User_feed.create ~max_users:2 ~initial_backoff:(sec 0.01) config >>| ok_exn
  in
  let%bind first = User_feed.subscribe feed ~address:leader >>| ok_exn in
  (* Snapshot: fills, BTC and ETH positions; then the live fill and BTC again. *)
  let%bind () = read_and_print ~label:"first" first 5 in
  [%expect
    {|
    (first
     (Fills (user 0x00000000000000000000000000000000000000aa) (is_snapshot true)
      (fills
       (((coin BTC) (px 100) (sz 0.01) (side Buy) (time 1) (start_position 0)
         (dir x) (closed_pnl 0) (hash 0x00) (oid 1) (crossed true) (fee 0)
         (tid 1) (fee_token ()) (cloid ()))
        ((coin ETH) (px 100) (sz 1) (side Sell) (time 2) (start_position 0)
         (dir x) (closed_pnl 0) (hash 0x00) (oid 1) (crossed true) (fee 0)
         (tid 2) (fee_token ()) (cloid ()))))))
    (first
     (Position (user 0x00000000000000000000000000000000000000aa) (coin BTC)
      (szi 0.01) (time 1)))
    (first
     (Position (user 0x00000000000000000000000000000000000000aa) (coin ETH)
      (szi -1) (time 2)))
    (first
     (Fills (user 0x00000000000000000000000000000000000000aa) (is_snapshot false)
      (fills
       (((coin BTC) (px 100) (sz 0.02) (side Buy) (time 3) (start_position 0.01)
         (dir x) (closed_pnl 0) (hash 0x00) (oid 1) (crossed true) (fee 0)
         (tid 3) (fee_token ()) (cloid ()))))))
    (first
     (Position (user 0x00000000000000000000000000000000000000aa) (coin BTC)
      (szi 0.03) (time 3)))
    |}];
  let%bind late = User_feed.subscribe feed ~address:leader >>| ok_exn in
  let%bind () = read_and_print ~label:"late" late 2 in
  [%expect
    {|
    (late
     (Position (user 0x00000000000000000000000000000000000000aa) (coin BTC)
      (szi 0.03) (time 3)))
    (late
     (Position (user 0x00000000000000000000000000000000000000aa) (coin ETH)
      (szi -1) (time 2)))
    |}];
  let%bind b = User_feed.subscribe feed ~address:other >>| ok_exn in
  let%bind () = read_and_print ~label:"other" b 2 in
  let%bind third =
    User_feed.subscribe
      feed
      ~address:(Address.of_string "0x00000000000000000000000000000000000000cc")
  in
  print_s [%sexp (Result.is_error third : bool)];
  print_s
    [%message
      ""
        ~users:(User_feed.users feed : Address.t list)
        ~leader_consumers:(User_feed.consumers feed ~address:leader : int)];
  [%expect
    {|
    (other
     (Fills (user 0x00000000000000000000000000000000000000bb) (is_snapshot true)
      (fills
       (((coin SOL) (px 100) (sz 3) (side Buy) (time 9) (start_position 0)
         (dir x) (closed_pnl 0) (hash 0x00) (oid 1) (crossed true) (fee 0)
         (tid 100) (fee_token ()) (cloid ()))))))
    (other
     (Position (user 0x00000000000000000000000000000000000000bb) (coin SOL)
      (szi 3) (time 9)))
    true
    ((users
      (0x00000000000000000000000000000000000000aa
       0x00000000000000000000000000000000000000bb))
     (leader_consumers 2))
    |}];
  (* The server drops the connection: Resync, then only the missed fill. *)
  let%bind () =
    Websocket.close
      (Queue.get server.sockets 0)
      ~code:Endpoint_going_away
      ~reason:"restart"
  in
  let%bind () = read_and_print ~label:"first" first 3 in
  let%bind () = read_and_print ~label:"late" late 3 in
  [%expect
    {|
    (first (Resync (user 0x00000000000000000000000000000000000000aa)))
    (first
     (Fills (user 0x00000000000000000000000000000000000000aa) (is_snapshot true)
      (fills
       (((coin BTC) (px 100) (sz 0.05) (side Sell) (time 4) (start_position 0.03)
         (dir x) (closed_pnl 0) (hash 0x00) (oid 1) (crossed true) (fee 0)
         (tid 4) (fee_token ()) (cloid ()))))))
    (first
     (Position (user 0x00000000000000000000000000000000000000aa) (coin BTC)
      (szi -0.02) (time 4)))
    (late (Resync (user 0x00000000000000000000000000000000000000aa)))
    (late
     (Fills (user 0x00000000000000000000000000000000000000aa) (is_snapshot true)
      (fills
       (((coin BTC) (px 100) (sz 0.05) (side Sell) (time 4) (start_position 0.03)
         (dir x) (closed_pnl 0) (hash 0x00) (oid 1) (crossed true) (fee 0)
         (tid 4) (fee_token ()) (cloid ()))))))
    (late
     (Position (user 0x00000000000000000000000000000000000000aa) (coin BTC)
      (szi -0.02) (time 4)))
    |}];
  Pipe.close_read first;
  Pipe.close_read late;
  let%bind () = Clock.after (sec 0.1) in
  print_s [%sexp (User_feed.users feed : Address.t list)];
  let%bind () = User_feed.close feed in
  let subscriptions =
    Queue.to_list server.received
    |> List.filter ~f:(fun (_, frame) -> String.is_substring frame ~substring:"userFills")
  in
  print_s [%sexp (subscriptions : (int * string) list)];
  let%bind () = Server.close server in
  [%expect
    {|
    (0x00000000000000000000000000000000000000bb)
    ((1
      "{\"method\":\"subscribe\",\"subscription\":{\"type\":\"userFills\",\"user\":\"0x00000000000000000000000000000000000000aa\"}}")
     (1
      "{\"method\":\"subscribe\",\"subscription\":{\"type\":\"userFills\",\"user\":\"0x00000000000000000000000000000000000000bb\"}}")
     (2
      "{\"method\":\"subscribe\",\"subscription\":{\"type\":\"userFills\",\"user\":\"0x00000000000000000000000000000000000000aa\"}}")
     (2
      "{\"method\":\"subscribe\",\"subscription\":{\"type\":\"userFills\",\"user\":\"0x00000000000000000000000000000000000000bb\"}}")
     (2
      "{\"method\":\"unsubscribe\",\"subscription\":{\"type\":\"userFills\",\"user\":\"0x00000000000000000000000000000000000000aa\"}}"))
    |}];
  return ()
;;
