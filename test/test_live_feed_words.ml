(* Minor words per frame that [Live_feed] adds on top of the websocket transport: frames
   read through the feed, minus the same frames read by a bare [Ws_client] that only
   counts them. Both sides pay the socket reads and async_websocket's framing, which
   differ a little between macOS arm64 and Linux amd64; the difference is the feed's own
   work (the frame hand-off, [Normalizer.frame] and the trades pipe). *)

open! Core
open Async
open Hl_client_blast

let n = 20_000

let mean results =
  List.sum (module Float) results ~f:(fun (r : Feed_cost.result) -> r.words)
  /. Float.of_int (List.length results)
;;

let%expect_test "words per frame added by the live feed" =
  Log.Global.set_output [];
  let%bind server = Blast_server.start ~frame:Feed_cost.frame ~per_write:1_000 () in
  let%bind floor = Feed_cost.floor server ~n ~runs:3 in
  let%bind feed = Feed_cost.live_feed server ~n ~runs:3 in
  print_s
    [%message
      "" ~feed_minus_floor:(Float.iround_nearest_exn (mean feed -. mean floor) : int)];
  (* 74 while the feed read [Ws_client.messages] one frame at a time; [on_message] drops
     the pipe hop's 13 words. *)
  [%expect {| (feed_minus_floor 61) |}];
  Blast_server.close server
;;
