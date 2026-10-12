(* Reconnection of [Ws] and [Live_feed] stepped with a synthetic clock: the reconnect
   delay only elapses when the test advances the clock. Socket I/O still runs in real
   time. *)

open! Core
open Async
open Hl_protocol
open Hl_client
module Server = Loopback_server

let wait_until f =
  let deadline = Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_sec 5.) in
  Deferred.repeat_until_finished () (fun () ->
    if f ()
    then return (`Finished true)
    else if Time_ns.( > ) (Time_ns.now ()) deadline
    then return (`Finished false)
    else Clock_ns.after (Time_ns.Span.of_int_ms 1) >>| fun () -> `Repeat ())
;;

(* The delay of the next alarm on [clock] if it is shorter than [below]: the reconnect
   sleep, ignoring the 50s keepalive. *)
let next_alarm_within clock ~below =
  match Time_source.next_alarm_fires_at clock with
  | None -> None
  | Some at ->
    let delay = Time_ns.diff at (Time_source.now clock) in
    Option.some_if Time_ns.Span.(delay < below) delay
;;

let wait_for_reconnect_sleep clock =
  let below = Time_ns.Span.of_sec 30. in
  let%map (_ : bool) =
    wait_until (fun () -> Option.is_some (next_alarm_within clock ~below))
  in
  next_alarm_within clock ~below
  |> Option.map ~f:(fun delay -> Float.iround_nearest_exn (Time_ns.Span.to_ms delay))
;;

(* Lets real I/O that should NOT happen have a chance to happen. *)
let settle () = Clock_ns.after (Time_ns.Span.of_int_ms 50)

let%expect_test "Ws reconnects only once the backoff has elapsed on the clock" =
  Log.Global.set_output [];
  let%bind server = Server.start ~greeting:[] () in
  let clock = Time_source.create ~now:Time_ns.epoch () in
  let%bind ws =
    Ws.connect
      ~time_source:(Time_source.read_only clock)
      ~initial_backoff:(Time_float.Span.of_sec 1.)
      ~backoff_jitter:0.
      ~on_reconnect:(fun () -> print_endline "on_reconnect")
      (Config.create ~base_url:(Server.base_url server) ())
    >>| ok_exn
  in
  let%bind () = Ws.subscribe ws All_mids in
  let%bind (_ : bool) = wait_until (fun () -> Queue.length server.received = 1) in
  let%bind () = Server.drop server 0 in
  let%bind (_ : bool) = wait_until (fun () -> not (Ws.is_connected ws)) in
  let%bind sleep_ms = wait_for_reconnect_sleep clock in
  print_s [%message (sleep_ms : int option)];
  let%bind () = Time_source.advance_by_alarms_by clock (Time_ns.Span.of_ms 990.) in
  let%bind () = settle () in
  print_s
    [%message
      "after 990ms"
        ~connections:(Queue.length server.sockets : int)
        ~reconnects:(Ws.reconnects ws : int)];
  let%bind () = Time_source.advance_by_alarms_by clock (Time_ns.Span.of_ms 10.) in
  let%bind reconnected = wait_until (fun () -> Ws.reconnects ws = 1) in
  let%bind (_ : bool) = wait_until (fun () -> Queue.length server.received = 2) in
  print_s
    [%message
      "after 1s"
        (reconnected : bool)
        ~connections:(Queue.length server.sockets : int)
        ~received:(Queue.to_list server.received : (int * string) list)];
  [%expect
    {|
    (sleep_ms (1000))
    ("after 990ms" (connections 1) (reconnects 0))
    on_reconnect
    ("after 1s" (reconnected true) (connections 2)
     (received
      ((1 "{\"method\":\"subscribe\",\"subscription\":{\"type\":\"allMids\"}}")
       (2 "{\"method\":\"subscribe\",\"subscription\":{\"type\":\"allMids\"}}"))))
    |}];
  let%bind () = Ws.close ws in
  Server.close server
;;

let book ~time =
  sprintf
    {|{"channel":"l2Book","data":{"coin":"BTC","time":%d,"levels":[[{"px":"100","sz":"1","n":1}],[{"px":"101","sz":"2","n":1}]]}}|}
    time
;;

let%expect_test "Live_feed stats count parse errors, error frames and reconnects" =
  Log.Global.set_output [];
  let on_subscribe ~connection (_ : Subscription.t) =
    if connection = 1
    then [ book ~time:1; {|{"channel":"l2Book","data":{"coin":"BTC"}}|} ]
    else [ book ~time:2 ]
  in
  let%bind server =
    Server.start
      ~greeting:[ {|{"channel":"error","data":"bad request"}|} ]
      ~on_subscribe
      ()
  in
  let clock = Time_source.create ~now:Time_ns.epoch () in
  let raw = Queue.create () in
  let on_raw ~stream ~recv_ts _frame = Queue.enqueue raw (stream, recv_ts) in
  let%bind (module Feed : Market_data.Feed.S), stats =
    Live_feed.create_with_stats
      ~on_raw
      ~time_source:(Time_source.read_only clock)
      (Config.create ~base_url:(Server.base_url server) ())
      ~symbols:[ "BTC" ]
      ~streams:[ Bbo; L2 ]
  in
  let%bind (_ : bool) = wait_until (fun () -> (stats ()).parse_errors = 1) in
  print_s [%sexp (stats () : Live_feed.Stats.t)];
  let%bind () = Server.drop server 0 in
  (* The venue-net default backoff is 0.5s with 20% jitter. *)
  let%bind sleep_ms = wait_for_reconnect_sleep clock in
  print_s
    [%message
      ""
        ~sleep_within_400_600ms:
          (Option.exists sleep_ms ~f:(fun ms -> ms >= 400 && ms <= 600) : bool)];
  let%bind () = Time_source.advance_by_alarms_by clock (Time_ns.Span.of_sec 1.) in
  let%bind (_ : bool) = wait_until (fun () -> (stats ()).frames = 3) in
  print_s [%sexp (stats () : Live_feed.Stats.t)];
  print_s
    [%sexp
      (Queue.to_list raw
       |> List.map ~f:(fun (stream, recv_ts) ->
         stream, Time_ns.to_int_ns_since_epoch recv_ts / 1_000_000)
       : (Market_data.Stream.t * int) list)];
  let%bind () = Feed.close () in
  let%bind l2 = Pipe.to_list Feed.l2 in
  let%bind bbo = Pipe.to_list Feed.bbo in
  (* The second connection's book has the same top, but each connection starts afresh. *)
  print_s [%message "" ~l2:(List.length l2 : int) ~bbo:(List.length bbo : int)];
  [%expect
    {|
    ((frames 2) (parse_errors 1) (error_frames 1) (reconnects 0))
    (sleep_within_400_600ms true)
    ((frames 3) (parse_errors 1) (error_frames 1) (reconnects 1))
    ((L2 0) (L2 0) (L2 1000))
    ((l2 2) (bbo 2))
    |}];
  Server.close server
;;

let ms time = Time_ns.to_int_ns_since_epoch time / 1_000_000

(* The venue-net default backoff after a successful connection: 0.5s with 20% jitter. *)
let within_backoff ms = ms >= 400 && ms <= 600

let%expect_test "Live_feed reports one Resync gap per reconnect, before the new snapshot" =
  Log.Global.set_output [];
  let on_subscribe ~connection (_ : Subscription.t) = [ book ~time:connection ] in
  let%bind server = Server.start ~greeting:[] ~on_subscribe () in
  let clock = Time_source.create ~now:Time_ns.epoch () in
  let%bind live =
    Live_feed.start
      ~time_source:(Time_source.read_only clock)
      (Config.create ~base_url:(Server.base_url server) ())
      ~symbols:[ "BTC" ]
      ~streams:[ L2 ]
  in
  let (module Feed : Market_data.Feed.With_gaps.S) = Live_feed.feed_with_gaps live in
  let%bind first = Pipe.read_exn Feed.l2 in
  (* Two reconnections, each stepped exactly to the end of its backoff sleep. *)
  let reconnect n =
    let%bind () = Server.drop server (n - 1) in
    let%bind (_ : int option) = wait_for_reconnect_sleep clock in
    let at = Option.value_exn (Time_source.next_alarm_fires_at clock) in
    let%bind () = Time_source.advance_by_alarms clock ~to_:at in
    let%bind gap = Pipe.read_exn Feed.gaps in
    let%map snapshot = Pipe.read_exn Feed.l2 in
    gap, snapshot
  in
  let%bind gap1, snapshot1 = reconnect 1 in
  let%bind gap2, snapshot2 = reconnect 2 in
  print_s
    [%message
      ""
        ~first_snapshot_ms:(ms first.recv_ts : int)
        ~venue:(gap1.venue : Market_data.Venue.t)
        ~instrument:(gap1.instrument : Market_data.Instrument.t option)
        ~stream:(gap1.stream : Market_data.Stream.t option)
        ~reason:(gap1.reason : Market_data.Gap.Reason.t)
        ~same_gap_twice:
          (Market_data.Gap.equal gap2 { gap1 with recv_ts = gap2.recv_ts } : bool)
        ~gap1_within_400_600ms:(within_backoff (ms gap1.recv_ts) : bool)
        ~snapshot1_at_gap:(Time_ns.equal snapshot1.recv_ts gap1.recv_ts : bool)
        ~gap2_within_400_600ms_of_gap1:
          (within_backoff (ms gap2.recv_ts - ms gap1.recv_ts) : bool)
        ~snapshot2_at_gap:(Time_ns.equal snapshot2.recv_ts gap2.recv_ts : bool)
        ~reconnects:((Live_feed.stats live).reconnects : int)];
  let%bind () = Feed.close () in
  let%bind rest = Pipe.to_list Feed.gaps in
  print_s [%message "after close" ~gaps:(List.length rest : int)];
  [%expect
    {|
    ((first_snapshot_ms 0) (venue Hyperliquid) (instrument ()) (stream ())
     (reason Resync) (same_gap_twice true) (gap1_within_400_600ms true)
     (snapshot1_at_gap true) (gap2_within_400_600ms_of_gap1 true)
     (snapshot2_at_gap true) (reconnects 2))
    ("after close" (gaps 0))
    |}];
  Server.close server
;;
