(* [Market_data.Rebuild_norm] over the recorded minute with [Raw_normalizer], then the
   event logs read back. *)

open! Core
open Async
open Hl_client
module Md = Market_data

let with_temp_dir f =
  let%bind dir = Unix.mkdtemp (Filename.temp_dir_name ^/ "hl-client-test") in
  Monitor.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      Process.run_expect_no_stdout_exn ~prog:"rm" ~args:[ "-rf"; dir ] ())
;;

let utc time = Time_ns.to_string_iso8601_basic time ~zone:Time_float.Zone.utc
let date = Date.of_string "2026-10-12"

let%expect_test "rebuild norm from the recorded minute" =
  with_temp_dir (fun dir ->
    let out_dir = dir ^/ "norm" in
    let%bind stats =
      Md.Rebuild_norm.run
        ~normalizers:[ (module Raw_normalizer) ]
        ~data_dir:"fixtures"
        ~out_dir
        date
    in
    print_s [%sexp (stats : Md.Rebuild_norm.Stats.t Or_error.t)];
    let%map () =
      Deferred.List.iter
        Md.Stream.[ Trades; Bbo; L2; Funding ]
        ~how:`Sequential
        ~f:(fun stream ->
          let path =
            Md.Event_log.path ~dir:out_dir (Normalizer.instrument "BTC") stream date
          in
          let%map events, tail = Md.Event_log.load_file path in
          let recv (e : Md.Event.t) = utc (Md.Event.recv_ts e) in
          print_s
            [%message
              ""
                ~file:(String.chop_prefix_exn path ~prefix:(dir ^ "/") : string)
                ~events:(List.length events : int)
                ~first:(recv (List.hd_exn events) : string)
                ~last:(recv (List.last_exn events) : string)
                (tail : Md.Framed.Tail.t)])
    in
    [%expect
      {|
      (Ok
       ((files 3) (frames 43) (disconnects 0) (events 64) (frame_errors 0)
        (first_errors ()) (skipped_files 0) (skipped_venues ())))
      ((file norm/hyperliquid/BTC/norm/trades/2026-10-12.bin) (events 40)
       (first 2026-10-12T13:35:01.515217000Z) (last 2026-10-12T13:35:19.252390000Z)
       (tail Clean))
      ((file norm/hyperliquid/BTC/norm/bbo/2026-10-12.bin) (events 6)
       (first 2026-10-12T13:35:03.466814000Z) (last 2026-10-12T13:35:30.325602000Z)
       (tail Clean))
      ((file norm/hyperliquid/BTC/norm/l2/2026-10-12.bin) (events 6)
       (first 2026-10-12T13:35:03.466814000Z) (last 2026-10-12T13:35:30.325602000Z)
       (tail Clean))
      ((file norm/hyperliquid/BTC/norm/funding/2026-10-12.bin) (events 12)
       (first 2026-10-12T13:35:00.275883000Z) (last 2026-10-12T13:35:11.735751000Z)
       (tail Clean))
      |}])
;;
