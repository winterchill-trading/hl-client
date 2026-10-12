(* A trimmed minute of mainnet frames in the market-recorder raw layout
   ([raw/<venue>/<stream>/<symbol>/<date>/<HH>-<MM>.ndjson], [<recv_ts> <frame>] per
   line), decoded and normalized. *)

open! Core
open Hl_client
module Md = Market_data

let dir stream =
  sprintf "fixtures/raw/hyperliquid/%s/BTC/2026/10/12" (Md.Stream.to_string stream)
;;

let streams : Md.Stream.t list = [ Trades; Bbo; L2; Funding ]

let lines stream =
  In_channel.read_lines (dir stream ^/ "13-35.ndjson")
  |> List.map ~f:(fun line ->
    let recv_ts, frame = String.lsplit2_exn line ~on:' ' in
    Time_ns.of_string_with_utc_offset recv_ts, frame)
;;

let%expect_test "every recorded frame decodes and normalizes" =
  let events = Hashtbl.create (module Md.Stream) in
  List.iter [ Md.Stream.Trades; L2; Funding ] ~f:(fun file_stream ->
    let frames = lines file_stream in
    let decode_errors =
      List.count frames ~f:(fun (_, frame) ->
        Or_error.is_error (Ws.For_testing.decode frame))
    in
    let normalize_errors = ref 0 in
    List.iter frames ~f:(fun (recv_ts, frame) ->
      match Live_feed.For_testing.normalize ~streams ~recv_ts frame with
      | Error _ -> incr normalize_errors
      | Ok normalized ->
        List.iter normalized ~f:(fun (event : Md.Event.t) ->
          let stream : Md.Stream.t =
            match event with
            | Trade _ -> Trades
            | Bbo _ -> Bbo
            | L2 _ -> L2
            | Funding _ -> Funding
          in
          Hashtbl.incr events stream));
    print_s
      [%message
        ""
          ~file:(file_stream : Md.Stream.t)
          ~frames:(List.length frames : int)
          (decode_errors : int)
          ~normalize_errors:(!normalize_errors : int)]);
  print_s
    [%sexp
      (Hashtbl.to_alist events |> List.sort ~compare:[%compare: Md.Stream.t * int]
       : (Md.Stream.t * int) list)];
  [%expect
    {|
    ((file Trades) (frames 25) (decode_errors 0) (normalize_errors 0))
    ((file L2) (frames 6) (decode_errors 0) (normalize_errors 0))
    ((file Funding) (frames 12) (decode_errors 0) (normalize_errors 0))
    ((Trades 40) (Bbo 6) (L2 6) (Funding 12))
    |}]
;;
