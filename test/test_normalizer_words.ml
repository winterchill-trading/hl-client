(* Words allocated by [Normalizer.frame] on one recorded frame of each stream, pinned so a
   change in the per-frame allocation shows up in review. Measured in the dev profile. *)

open! Core
open Hl_client
module Md = Market_data

let first_frame stream =
  In_channel.read_lines
    (sprintf
       "fixtures/raw/hyperliquid/%s/BTC/2026/10/12/13-35.ndjson"
       (Md.Stream.to_string stream))
  |> List.hd_exn
  |> String.lsplit2_exn ~on:' '
  |> snd
;;

let words ?streams stream =
  let frame = first_frame stream in
  let t = Normalizer.create ?streams () in
  let run () =
    Normalizer.reset t;
    ignore (Normalizer.frame t ~recv_ts:Time_ns.epoch frame : Md.Event.t list Or_error.t)
  in
  (* Warm up: the first call fills the bbo table and any lazily built values. *)
  run ();
  let #((), report) = Gc.For_testing.measure_allocation run in
  report.minor_words_allocated + report.major_words_allocated
;;

let%expect_test "words per frame" =
  print_s
    [%message
      ""
        ~trades:(words Trades : int)
        ~l2:(words L2 : int)
        ~bbo_only:(words ~streams:[ Bbo ] L2 : int)
        ~funding:(words Funding : int)];
  [%expect {| ((trades 45) (l2 271) (bbo_only 19) (funding 411)) |}]
;;
