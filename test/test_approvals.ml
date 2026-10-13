(* Approvals and builder orders posted to a capturing HTTP server; every signature is
   recovered from the body that went over the wire. *)

open! Core
open Async
open Hl_protocol
open Hl_client

let key =
  Signing.Private_key.of_hex
    "0x0123456789012345678901234567890123456789012345678901234567890123"
  |> Or_error.ok_exn
;;

let agent = Address.of_string "0x00000000000000000000000000000000000000a1"
let builder = Address.of_string "0x00000000000000000000000000000000000000b1"

(* [place_orders] takes its nonce from the clock, so the nonce and the signature over it
   are masked once the signer has been recovered. *)
let mask body =
  match Yojson.Safe.from_string body with
  | `Assoc fields ->
    `Assoc
      (List.map fields ~f:(fun (key, value) ->
         match key with
         | "nonce" -> key, `String "<nonce>"
         | "signature" -> key, `String "<signature>"
         | _ -> key, value))
    |> Yojson.Safe.to_string
  | _ -> body
;;

let with_server ?(masked = false) f =
  let%bind server = Http_capture.start () in
  let config = Config.create ~base_url:(Http_capture.base_url server) () in
  let%bind () = f config in
  List.iter (Http_capture.requests server) ~f:(fun { path; x_address; body } ->
    let json = Yojson.Safe.from_string body in
    let signer =
      match User_signed_request.is_user_signed_body json with
      | true ->
        let request = User_signed_request.t_of_yojson json in
        Signing.recover_user_action
          request.action
          (Signing.Signature.t_of_yojson (Option.value_exn request.signature))
      | false ->
        let envelope = Exchange_envelope.t_of_yojson json in
        Signing.recover
          ~digest:
            (Signing.l1_digest
               ~is_mainnet:false
               ~connection_id:
                 (Signing.action_hash
                    ~vault_address:envelope.vault_address
                    ~nonce:envelope.nonce
                    envelope.action))
          (Signing.Signature.t_of_yojson (Option.value_exn envelope.signature))
    in
    print_s
      [%message path (x_address : string option) ~signer:(signer : Address.t Or_error.t)];
    print_endline (if masked then mask body else body));
  Http_capture.close server
;;

let%expect_test "approveAgent and approveBuilderFee bodies" =
  let%bind () =
    with_server (fun config ->
      let%bind response =
        Exchange.approve_agent
          config
          ~key
          ~hyperliquid_chain:Testnet
          ~nonce:1700000000000
          ~agent_address:agent
          ~agent_name:"copy"
          ()
      in
      print_s [%sexp (response : Exchange_response.t Or_error.t)];
      let%map response =
        Exchange.approve_builder_fee
          config
          ~key
          ~hyperliquid_chain:Testnet
          ~nonce:1700000000001
          ~builder
          ~max_fee_tenths_bp:10
          ()
      in
      print_s [%sexp (response : Exchange_response.t Or_error.t)])
  in
  [%expect
    {|
    (Ok (Ok Default))
    (Ok (Ok Default))
    (/exchange (x_address (0x14791697260e4c9a71f18484c9f997b308e59325))
     (signer (Ok 0x14791697260e4c9a71f18484c9f997b308e59325)))
    {"action":{"type":"approveAgent","hyperliquidChain":"Testnet","signatureChainId":"0x66eee","agentAddress":"0x00000000000000000000000000000000000000a1","agentName":"copy","nonce":1700000000000},"nonce":1700000000000,"signature":{"r":"0x9c42d3a4bdfc708ec2d0e5299bf42916c02b84d0d5d212e46029a99f7a2289c4","s":"0x73a3df73e690b0d85e7e51d3507059b5d02737d6acdf293e29ddacb3b4a074a4","v":27}}
    (/exchange (x_address (0x14791697260e4c9a71f18484c9f997b308e59325))
     (signer (Ok 0x14791697260e4c9a71f18484c9f997b308e59325)))
    {"action":{"type":"approveBuilderFee","hyperliquidChain":"Testnet","signatureChainId":"0x66eee","maxFeeRate":"0.01%","builder":"0x00000000000000000000000000000000000000b1","nonce":1700000000001},"nonce":1700000000001,"signature":{"r":"0x9ddd0dd6991e7d7101b7bf259f6ec0476229b89d3f41ea6d536cb32d5aa10a22","s":"0x3f8eb44489b650bfe570c6e61750c021b0e6ad0c79568479096a2cc7d0420729","v":27}}
    |}];
  return ()
;;

let%expect_test "builder order is signed over the action with the builder" =
  let%bind () =
    with_server ~masked:true (fun config ->
      let%map response =
        Exchange.place_orders
          config
          ~sign:(Signing.sign_request key ~is_mainnet:false)
          ~builder:{ builder; fee_tenths_bp = 10 }
          [ { Order_request.asset = Asset.of_int 0
            ; is_buy = true
            ; limit_px = Price.of_string "100"
            ; sz = Size.of_string "1"
            ; reduce_only = false
            ; order_type = Order_type.ioc
            ; cloid = None
            }
          ]
      in
      print_s [%sexp (response : Exchange_response.t Or_error.t)])
  in
  [%expect
    {|
    (Ok (Ok Default))
    (/exchange (x_address ())
     (signer (Ok 0x14791697260e4c9a71f18484c9f997b308e59325)))
    {"action":{"type":"order","orders":[{"a":0,"b":true,"p":"100","s":"1","r":false,"t":{"limit":{"tif":"Ioc"}}}],"grouping":"na","builder":{"b":"0x00000000000000000000000000000000000000b1","f":10}},"nonce":"<nonce>","signature":"<signature>"}
    |}];
  return ()
;;

let%expect_test "agent names" =
  let now = Time_ns.of_int_ns_since_epoch 1_760_000_000_000_000_000 in
  List.iter
    [ "copy", None
    ; "copy", Some (Time_ns.Span.of_day 30.)
    ; "copy", Some (Time_ns.Span.of_day 181.)
    ; "copy", Some (Time_ns.Span.of_sec (-1.))
    ; " ", None
    ]
    ~f:(fun (name, ahead) ->
      let valid_until = Option.map ahead ~f:(Time_ns.add now) in
      print_s [%sexp (Exchange.agent_name ~now ?valid_until name : string Or_error.t)]);
  [%expect
    {|
    (Ok copy)
    (Ok "copy valid_until 1762592000000")
    (Error ("valid_until is more than 180 days ahead" (until_ms 1775638400000)))
    (Error ("valid_until is not in the future" (until_ms 1759999999000)))
    (Error "agent name must not be empty")
    |}];
  return ()
;;

let agent_key = Signing.Private_key.of_hex (String.make 63 '0' ^ "2") |> Or_error.ok_exn
let account = Signing.Private_key.address key

let%expect_test "agent handle: orders signed by the agent for the account" =
  let%bind () =
    with_server ~masked:true (fun config ->
      let t =
        Client.trading_with_agent
          ~is_mainnet:false
          ~agent_key
          ~account_address:account
          config
      in
      print_s
        [%message
          ""
            ~agent_address:(Signing.Private_key.address agent_key : Address.t)
            ~account:(Client.account t : Address.t option)
            ~is_agent:(Client.is_agent t : bool)];
      let%bind response =
        Client.cancel t [ { asset = Asset.of_int 0; oid = Order_id.of_int 1 } ]
      in
      print_s [%sexp (response : Exchange_response.t Or_error.t)];
      let%map refused = Client.approve_agent t ~agent_address:agent ~agent_name:"copy" in
      print_s [%sexp (refused : Exchange_response.t Or_error.t)])
  in
  [%expect
    {|
    ((agent_address 0x2b5ad5c4795c026514f8317c7a215e218dccd6cf)
     (account (0x14791697260e4c9a71f18484c9f997b308e59325)) (is_agent true))
    (Ok (Ok Default))
    (Error "approvals must be signed by the account's own key, not by an agent")
    (/exchange (x_address (0x14791697260e4c9a71f18484c9f997b308e59325))
     (signer (Ok 0x2b5ad5c4795c026514f8317c7a215e218dccd6cf)))
    {"action":{"type":"cancel","cancels":[{"a":0,"o":1}]},"nonce":"<nonce>","signature":"<signature>"}
    |}];
  return ()
;;

let%expect_test "owner handle approves through Client" =
  let%bind () =
    with_server (fun config ->
      let t = Client.trading ~key ~is_mainnet:false config in
      let%bind response =
        Client.approve_agent t ~nonce:1 ~agent_address:agent ~agent_name:"copy"
      in
      print_s [%sexp (response : Exchange_response.t Or_error.t)];
      let%map response =
        Client.approve_builder_fee t ~nonce:2 ~builder ~max_fee_tenths_bp:100
      in
      print_s [%sexp (response : Exchange_response.t Or_error.t)])
  in
  [%expect
    {|
    (Ok (Ok Default))
    (Ok (Ok Default))
    (/exchange (x_address (0x14791697260e4c9a71f18484c9f997b308e59325))
     (signer (Ok 0x14791697260e4c9a71f18484c9f997b308e59325)))
    {"action":{"type":"approveAgent","hyperliquidChain":"Testnet","signatureChainId":"0x66eee","agentAddress":"0x00000000000000000000000000000000000000a1","agentName":"copy","nonce":1},"nonce":1,"signature":{"r":"0xa4213abaefe87cb544390cd8935f33ea18e6b83b9bf98f57fbe572f43253498a","s":"0x5cc8cd7f7094e93df578d6bd57116dfc3939965975f44ef2a83f6eb995754618","v":28}}
    (/exchange (x_address (0x14791697260e4c9a71f18484c9f997b308e59325))
     (signer (Ok 0x14791697260e4c9a71f18484c9f997b308e59325)))
    {"action":{"type":"approveBuilderFee","hyperliquidChain":"Testnet","signatureChainId":"0x66eee","maxFeeRate":"0.1%","builder":"0x00000000000000000000000000000000000000b1","nonce":2},"nonce":2,"signature":{"r":"0xaa90a116d0340476c5c52a77daf0e8da06cc13507d59be8e7331b31ca5eda5ea","s":"0x2bfb84736b535037e7138c90f9dc4f9bc1492fd3cf60d98ded76db5524cfa555","v":28}}
    |}];
  return ()
;;
