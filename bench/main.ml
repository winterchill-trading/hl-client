(* Normalizer per frame on the recorded mainnet frames (test/fixtures): each test cycles
   through the frames of one stream, one frame per run. [l2] is a full 20-level book per
   frame (an L2 snapshot and a bbo); [bbo only] is the same frames with only the Bbo
   stream wanted. *)

open! Core
open Core_bench
open Hl_client
module Md = Market_data

let frames text =
  String.split_lines text
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(fun line -> snd (String.lsplit2_exn line ~on:' '))
  |> Array.of_list
;;

let per_frame ?streams name text =
  let frames = frames text in
  let t = Normalizer.create ?streams () in
  let i = ref 0 in
  Bench.Test.create ~name (fun () ->
    let frame = frames.(!i) in
    i := (!i + 1) % Array.length frames;
    (* Forget the last top so every l2 frame does the same work. *)
    Normalizer.reset t;
    ignore (Normalizer.frame t ~recv_ts:Time_ns.epoch frame : Md.Event.t list Or_error.t))
;;

let () =
  Command_unix.run
    (Bench.make_command
       [ per_frame "trades" Frames.trades
       ; per_frame "l2" Frames.l2
       ; per_frame ~streams:[ Bbo ] "bbo only" Frames.l2
       ; per_frame "funding" Frames.funding
       ])
;;
