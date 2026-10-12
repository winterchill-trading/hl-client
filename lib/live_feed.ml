open! Core
open! Async
module Md = Market_data
module Ws_client = Venue_net.Ws_client

let wants streams stream = List.mem streams stream ~equal:Md.Stream.equal

let subscription_json ~type_ ~coin =
  `Assoc
    [ "method", `String "subscribe"
    ; "subscription", `Assoc [ "type", `String type_; "coin", `String coin ]
    ]
  |> Yojson.Safe.to_string
;;

(* Hl_protocol.Subscription has no activeAssetCtx, so all requests are built here. *)
let subscribe_requests ~symbols ~streams =
  let types =
    List.concat
      [ (if wants streams Trades then [ "trades" ] else [])
      ; (if wants streams L2 || wants streams Bbo then [ "l2Book" ] else [])
      ; (if wants streams Funding then [ "activeAssetCtx" ] else [])
      ]
  in
  List.concat_map symbols ~f:(fun coin ->
    List.map types ~f:(fun type_ -> subscription_json ~type_ ~coin))
;;

let channel = Normalizer.channel
let streams_of_channel = Normalizer.streams_of_channel

(* An l2Book frame feeds both L2 and Bbo; it is recorded once, under the first. *)
let raw_stream ~streams channel = List.hd (streams_of_channel ~streams channel)

let normalize ~streams ~recv_ts frame =
  Normalizer.frame (Normalizer.create ~streams ()) ~recv_ts frame
;;

module Pipes = struct
  type t =
    { trades : Md.Trade.t Pipe.Writer.t
    ; bbo : Md.Bbo.t Pipe.Writer.t
    ; l2 : Md.L2_update.t Pipe.Writer.t
    ; funding : Md.Funding.t Pipe.Writer.t
    }

  let write t : Md.Event.t -> unit = function
    | Trade x -> Pipe.write_without_pushback_if_open t.trades x
    | Bbo x -> Pipe.write_without_pushback_if_open t.bbo x
    | L2 x -> Pipe.write_without_pushback_if_open t.l2 x
    | Funding x -> Pipe.write_without_pushback_if_open t.funding x
  ;;

  let close t =
    Pipe.close t.trades;
    Pipe.close t.bbo;
    Pipe.close t.l2;
    Pipe.close t.funding
  ;;
end

module Stats = struct
  type t =
    { frames : int
    ; parse_errors : int
    ; error_frames : int
    ; reconnects : int
    }
  [@@deriving sexp_of]
end

module Counters = struct
  type t =
    { mutable frames : int
    ; mutable parse_errors : int
    ; mutable error_frames : int
    }
end

let create_with_stats
  ?(on_raw = fun ~stream:_ ~recv_ts:_ _ -> ())
  ?time_source
  (config : Config.t)
  ~symbols
  ~streams
  =
  let requests = subscribe_requests ~symbols ~streams in
  let on_connect client =
    Deferred.all_unit (List.map requests ~f:(Ws_client.send client))
  in
  let%map client =
    Ws_client.connect
      ~ping:(`Text ({|{"method":"ping"}|}, Time_ns.Span.of_sec 50.))
      ~max_silent_intervals:1
      ~on_connect
      ~log_name:"hl-client live feed"
      ?time_source
      ~url:config.ws_url
      ()
    >>| ok_exn
  in
  let trades_r, trades = Pipe.create () in
  let bbo_r, bbo = Pipe.create () in
  let l2_r, l2 = Pipe.create () in
  let funding_r, funding = Pipe.create () in
  let pipes = { Pipes.trades; bbo; l2; funding } in
  if not (wants streams Trades) then Pipe.close trades;
  if not (wants streams Bbo) then Pipe.close bbo;
  if not (wants streams L2) then Pipe.close l2;
  if not (wants streams Funding) then Pipe.close funding;
  let normalizer = Normalizer.create ~streams () in
  let counters = { Counters.frames = 0; parse_errors = 0; error_frames = 0 } in
  let now () =
    match time_source with
    | None -> Time_ns.now ()
    | Some time_source -> Time_source.now time_source
  in
  let on_frame frame =
    let recv_ts = now () in
    match channel frame with
    | Some "error" ->
      counters.error_frames <- counters.error_frames + 1;
      Log.Global.error_s [%message "hl-client live feed: error frame" (frame : string)]
    | channel ->
      (match raw_stream ~streams (Option.value channel ~default:"") with
       | None -> ()
       | Some stream ->
         counters.frames <- counters.frames + 1;
         on_raw ~stream ~recv_ts frame;
         (match Normalizer.frame normalizer ~recv_ts frame with
          | Ok events -> List.iter events ~f:(Pipes.write pipes)
          | Error error ->
            counters.parse_errors <- counters.parse_errors + 1;
            Log.Global.error_s
              [%message
                "hl-client live feed: unparsable frame" (frame : string) (error : Error.t)]))
  in
  let stats () =
    { Stats.frames = counters.frames
    ; parse_errors = counters.parse_errors
    ; error_frames = counters.error_frames
    ; reconnects = Ws_client.reconnects client
    }
  in
  don't_wait_for
    (let%map () = Pipe.iter_without_pushback (Ws_client.messages client) ~f:on_frame in
     Pipes.close pipes);
  let module Feed = struct
    let trades = trades_r
    let bbo = bbo_r
    let l2 = l2_r
    let funding = funding_r

    let close () =
      let%map () = Ws_client.close client in
      Pipes.close pipes
    ;;
  end
  in
  (module Feed : Md.Feed.S), stats
;;

let create ?on_raw ?time_source config ~symbols ~streams =
  create_with_stats ?on_raw ?time_source config ~symbols ~streams >>| fst
;;

module For_testing = struct
  let subscribe_requests = subscribe_requests
  let channel = channel
  let streams_of_channel = streams_of_channel
  let raw_stream = raw_stream
  let normalize = normalize
end
