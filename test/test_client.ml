open! Core
open Hl_protocol
open Hl_client

let key =
  Signing.Private_key.of_hex
    "0x0123456789012345678901234567890123456789012345678901234567890123"
  |> Or_error.ok_exn
;;

let%expect_test "handles" =
  print_s (Client.sexp_of_t (Client.read_only (Config.mainnet ())));
  print_s (Client.sexp_of_t (Client.trading ~key (Config.testnet ())));
  print_s (Client.sexp_of_t (Client.trading (Config.local ())));
  [%expect
    {|
    ((config
      ((base_url https://api.hyperliquid.xyz)
       (ws_url wss://api.hyperliquid.xyz/ws) (address ())))
     (signed false) (network mainnet))
    ((config
      ((base_url https://api.hyperliquid-testnet.xyz)
       (ws_url wss://api.hyperliquid-testnet.xyz/ws)
       (address (0x14791697260e4c9a71f18484c9f997b308e59325))))
     (signed true) (network testnet))
    ((config
      ((base_url http://localhost:3000) (ws_url ws://localhost:3000/ws)
       (address ())))
     (signed false) (network testnet))
    |}]
;;

let%expect_test "trading requests are signed and recover to the key's address" =
  let t = Client.trading ~key (Config.testnet ()) in
  let action = Exchange_action.Cancel { cancels = [] } in
  let request = Client.build_request t ~nonce:42 action in
  let signature = Signing.Signature.t_of_yojson (Option.value_exn request.signature) in
  let digest =
    Signing.l1_digest
      ~is_mainnet:false
      ~connection_id:
        (Signing.action_hash
           ~vault_address:None
           ~nonce:42
           (Exchange_action.yojson_of_t action))
  in
  print_s [%sexp (Signing.recover ~digest signature : Address.t Or_error.t)];
  let unsigned =
    Client.build_request (Client.trading (Config.local ())) ~nonce:42 action
  in
  print_s [%sexp (unsigned.signature : Json.t option)];
  [%expect {|
    (Ok 0x14791697260e4c9a71f18484c9f997b308e59325)
    ()
    |}]
;;

let%expect_test "trading_of_env" =
  print_s
    ([%sexp_of: Sexp.t Or_error.t]
       (Client.trading_of_env ~var:"HL_CLIENT_TEST_UNSET_KEY" (Config.local ())
        |> Or_error.map ~f:Client.sexp_of_t));
  [%expect {| (Error ("environment variable not set" HL_CLIENT_TEST_UNSET_KEY)) |}]
;;
