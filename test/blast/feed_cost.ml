open! Core
open Async
open Hl_client
module Ws_client = Venue_net.Ws_client

type result =
  { seconds : float
  ; words : float
  }

let frame =
  {|{"channel":"trades","data":[{"coin":"BTC","side":"A","px":"82525.0","sz":"0.01671","time":1791812100812,"hash":"0x6b5994a6ac828c606cd3044666b632020966008c4785ab320f223ff96b86664b","tid":112328450464875,"users":["0x88b29ca555c188c98f3cb0ae6f662ad7840e184e","0xf5d81a135f756ca16544e53c20fc20643ec3ad53"]}]}|}
;;

let url server = Uri.of_string (sprintf "ws://127.0.0.1:%d/ws" (Blast_server.port server))

(* [count] is advanced by the consumer; [done_] is filled by it at [n]. *)
type progress =
  { mutable count : int
  ; mutable target : int
  ; mutable done_ : unit Ivar.t
  }

let advance p k =
  p.count <- p.count + k;
  if p.count >= p.target then Ivar.fill_if_empty p.done_ ()
;;

let measure server p ~n =
  p.count <- 0;
  p.target <- n;
  p.done_ <- Ivar.create ();
  Gc.full_major ();
  let words0 = Gc.minor_words () in
  let t0 = Time_ns.now () in
  let%bind (_ : int) = Blast_server.blast server ~n in
  let%map () = Ivar.read p.done_ in
  let t1 = Time_ns.now () in
  let words1 = Gc.minor_words () in
  { seconds = Time_ns.Span.to_sec (Time_ns.diff t1 t0)
  ; words = Float.of_int (words1 - words0) /. Float.of_int n
  }
;;

let runs_of server p ~n ~runs =
  let%bind (_ : result) = measure server p ~n in
  Deferred.List.init runs ~how:`Sequential ~f:(fun _ -> measure server p ~n)
;;

let progress () = { count = 0; target = Int.max_value; done_ = Ivar.create () }

let floor server ~n ~runs =
  let p = progress () in
  let%bind client =
    Ws_client.connect ~on_message:(fun (_ : string) -> advance p 1) ~url:(url server) ()
    >>| ok_exn
  in
  let%bind () = Ws_client.send client "hello" in
  let%bind () = Blast_server.ready server in
  let%bind results = runs_of server p ~n ~runs in
  let%map () = Ws_client.close client in
  results
;;

let live_feed server ~n ~runs =
  let p = progress () in
  let%bind (module Feed : Market_data.Feed.S) =
    Live_feed.create
      (Config.create ~base_url:(Uri.of_string "http://unused") ~ws_url:(url server) ())
      ~symbols:[ "BTC" ]
      ~streams:[ Trades ]
  in
  don't_wait_for
    (Pipe.iter' Feed.trades ~f:(fun batch ->
       advance p (Queue.length batch);
       return ()));
  let%bind () = Blast_server.ready server in
  let%bind results = runs_of server p ~n ~runs in
  let%map () = Feed.close () in
  results
;;
