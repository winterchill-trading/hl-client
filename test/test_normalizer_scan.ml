(* The scanner path of [Normalizer.frame] against the Yojson path
   ([Normalizer.For_testing.frame_yojson]): identical events, frame by frame, over every
   recorded frame and over reshaped copies of them (whitespace, key order, deep books),
   with the per-instrument bbo state carried along on both sides. *)

open! Core
open Hl_client
module Md = Market_data

let frames stream =
  In_channel.read_lines
    (sprintf
       "fixtures/raw/hyperliquid/%s/BTC/2026/10/12/13-35.ndjson"
       (Md.Stream.to_string stream))
  |> List.map ~f:(fun line ->
    let recv_ts, frame = String.lsplit2_exn line ~on:' ' in
    Time_ns.of_string_with_utc_offset recv_ts, frame)
;;

let recorded () = List.concat_map Md.Stream.[ Trades; L2; Funding ] ~f:frames

let stream_sets : Md.Stream.t list list =
  [ [ Trades; Bbo; L2; Funding ]; [ Bbo ]; [ L2 ]; [ Trades ]; [] ]
;;

(* Runs [frames] through both paths; prints the frames that differ and a count. *)
let compare_paths frames =
  List.iter stream_sets ~f:(fun streams ->
    let scan = Normalizer.create ~streams () in
    let reference = Normalizer.create ~streams () in
    let events = ref 0 in
    let differ = ref 0 in
    List.iter frames ~f:(fun (recv_ts, frame) ->
      let a = Normalizer.frame scan ~recv_ts frame in
      let b = Normalizer.For_testing.frame_yojson reference ~recv_ts frame in
      match a, b with
      | Ok a, Ok b when [%equal: Md.Event.t list] a b -> events := !events + List.length a
      | Error _, Error _ -> ()
      | _ ->
        incr differ;
        print_s [%message "differ" frame]);
    print_s
      [%message
        ""
          (streams : Md.Stream.t list)
          ~frames:(List.length frames : int)
          (events : int ref)
          (differ : int ref)])
;;

let%expect_test "every recorded frame, both paths" =
  compare_paths (recorded ());
  [%expect
    {|
    ((streams (Trades Bbo L2 Funding)) (frames 43) (events 64) (differ 0))
    ((streams (Bbo)) (frames 43) (events 6) (differ 0))
    ((streams (L2)) (frames 43) (events 6) (differ 0))
    ((streams (Trades)) (frames 43) (events 40) (differ 0))
    ((streams ()) (frames 43) (events 0) (differ 0))
    |}]
;;

(* Re-serialized through Yojson with spaces and newlines, and with the keys of every
   object reversed: the scanner may not depend on the recorded layout. *)
let rec reverse_keys : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Assoc kvs -> `Assoc (List.rev_map kvs ~f:(fun (k, v) -> k, reverse_keys v))
  | `List xs -> `List (List.map xs ~f:reverse_keys)
  | x -> x
;;

let reshaped ~f =
  List.map (recorded ()) ~f:(fun (recv_ts, frame) ->
    recv_ts, Yojson.Safe.pretty_to_string (f (Yojson.Safe.from_string frame)))
;;

let%expect_test "reshaped frames, both paths" =
  compare_paths (reshaped ~f:Fn.id @ reshaped ~f:reverse_keys);
  [%expect
    {|
    ((streams (Trades Bbo L2 Funding)) (frames 86) (events 128) (differ 0))
    ((streams (Bbo)) (frames 86) (events 12) (differ 0))
    ((streams (L2)) (frames 86) (events 12) (differ 0))
    ((streams (Trades)) (frames 86) (events 80) (differ 0))
    ((streams ()) (frames 86) (events 0) (differ 0))
    |}]
;;

(* Frames the scanner refuses go to the Yojson path and give what it gives. *)
let%expect_test "refused shapes" =
  let book side_levels =
    sprintf
      {|{"channel":"l2Book","data":{"coin":"ETH","time":1,"levels":[%s]}}|}
      side_levels
  in
  let level px = sprintf {|{"px":"%s","sz":"1","n":1}|} px in
  let deep = List.init 300 ~f:(fun i -> level (Int.to_string (1000 - i))) in
  let at = Time_ns.epoch in
  compare_paths
    [ at, book (sprintf "[%s],[%s]" (level "10") (level "11"))
    ; at, book (sprintf "[%s],[%s]" (level "1e1") (level "11"))
    ; at, book (sprintf "[%s],[%s]" (level "-1") (level "11"))
    ; at, book (sprintf "[%s]" (level "10"))
    ; at, book (sprintf "[%s],[]" (String.concat ~sep:"," deep))
    ; ( at
      , {|{"channel":"trades","data":[{"coin":"E\u0054H","side":"B","px":"1","sz":"1","time":1,"hash":"0x","tid":1,"users":["0x1","0x2"]}]}|}
      )
    ; at, {|{"channel":"trades","data":[]}|}
    ; at, {|{"channel":"trades","data":{}}|}
    ];
  [%expect
    {|
    ((streams (Trades Bbo L2 Funding)) (frames 8) (events 6) (differ 0))
    ((streams (Bbo)) (frames 8) (events 2) (differ 0))
    ((streams (L2)) (frames 8) (events 3) (differ 0))
    ((streams (Trades)) (frames 8) (events 1) (differ 0))
    ((streams ()) (frames 8) (events 0) (differ 0))
    |}]
;;
