(* Command-line smoke tests for hl-client, e.g. against the simulator:

   hl-cli l2book BTC hl-cli mids -base-url https://api.hyperliquid.xyz hl-cli watch
   -l2book BTC -trades BTC -all-mids *)

open! Core
open Async
open Hl_protocol
open Hl_client

let config_param =
  let%map_open.Command base_url =
    flag
      "base-url"
      (optional_with_default "http://localhost:3000" string)
      ~doc:"URL HTTP API root (default http://localhost:3000)"
  and ws_url =
    flag "ws-url" (optional string) ~doc:"URL websocket endpoint (default: derived)"
  and debug = flag "debug" no_arg ~doc:" log at debug level" in
  if debug then Log.Global.set_level `Debug;
  Config.create
    ?ws_url:(Option.map ws_url ~f:Uri.of_string)
    ~base_url:(Uri.of_string base_url)
    ()
;;

let print_result sexp_of = function
  | Ok value ->
    print_s (sexp_of value);
    return ()
  | Error error ->
    Print.eprint_s [%sexp (error : Error.t)];
    exit 1
;;

let l2book_command =
  Command.async
    ~summary:"Print an L2 book snapshot from /info"
    (let%map_open.Command config = config_param
     and coin = anon ("COIN" %: string)
     and n_sig_figs = flag "n-sig-figs" (optional int) ~doc:"N aggregate to N sig figs" in
     fun () ->
       Info.l2_book ?n_sig_figs config (Coin.of_string coin)
       >>= print_result [%sexp_of: L2_book.t])
;;

let mids_command =
  Command.async
    ~summary:"Print all mid prices from /info"
    (let%map_open.Command config = config_param in
     fun () -> Info.all_mids config >>= print_result [%sexp_of: All_mids.t])
;;

let watch_command =
  Command.async
    ~summary:"Subscribe over the websocket and print every message"
    (let%map_open.Command config = config_param
     and l2book = flag "l2book" (listed string) ~doc:"COIN subscribe to l2Book"
     and trades = flag "trades" (listed string) ~doc:"COIN subscribe to trades"
     and all_mids = flag "all-mids" no_arg ~doc:" subscribe to allMids"
     and order_updates =
       flag "order-updates" (listed string) ~doc:"ADDRESS subscribe to orderUpdates"
     and user_fills =
       flag "user-fills" (listed string) ~doc:"ADDRESS subscribe to userFills"
     in
     fun () ->
       let subscriptions =
         List.concat
           [ List.map l2book ~f:(fun coin ->
               Subscription.L2_book { coin = Coin.of_string coin })
           ; List.map trades ~f:(fun coin ->
               Subscription.Trades { coin = Coin.of_string coin })
           ; (if all_mids then [ Subscription.All_mids ] else [])
           ; List.map order_updates ~f:(fun user ->
               Subscription.Order_updates { user = Address.of_string user })
           ; List.map user_fills ~f:(fun user ->
               Subscription.User_fills { user = Address.of_string user })
           ]
       in
       if List.is_empty subscriptions
       then (
         Print.eprintf "nothing to watch; pass at least one subscription flag\n";
         exit 2)
       else (
         let%bind ws =
           Ws.connect ~on_reconnect:(fun () -> Print.eprintf "reconnected\n") config
           >>| ok_exn
         in
         let%bind () =
           Deferred.List.iter subscriptions ~how:`Sequential ~f:(Ws.subscribe ws)
         in
         Pipe.iter_without_pushback (Ws.messages ws) ~f:(fun message ->
           print_s [%sexp (message : Ws_message.t)])))
;;

let () =
  Command.group
    ~summary:"Hyperliquid API client"
    [ "l2book", l2book_command; "mids", mids_command; "watch", watch_command ]
  |> Command_unix.run
;;
