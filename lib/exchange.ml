open! Core
open Async
open Hl_protocol

type sign = Yojson.Safe.t -> Yojson.Safe.t option

let last_nonce = ref 0

let next_nonce () =
  let now_ms = Time_ns.to_int_ns_since_epoch (Time_ns.now ()) / 1_000_000 in
  let nonce = Int.max now_ms (!last_nonce + 1) in
  last_nonce := nonce;
  nonce
;;

let build_request ?sign ?vault_address ~nonce action =
  let unsigned = { Exchange_request.action; nonce; signature = None; vault_address } in
  match sign with
  | None -> unsigned
  | Some sign ->
    { unsigned with signature = sign (Exchange_request.yojson_of_t unsigned) }
;;

let decode (response : Http.Response.t) =
  match
    Or_error.try_with (fun () ->
      Exchange_response.t_of_yojson (Yojson.Safe.from_string response.body))
  with
  | Ok _ as ok -> ok
  | Error _ as error ->
    if response.status >= 200 && response.status < 300
    then
      Or_error.tag_s
        error
        ~tag:[%message "" ~status:(response.status : int) ~body:(response.body : string)]
    else Http.decode_json response ~f:Exchange_response.t_of_yojson
;;

let headers (config : Config.t) =
  match config.address with
  | None -> []
  | Some address -> [ "X-Address", Address.to_string address ]
;;

let post config ?sign ?vault_address action =
  let request = build_request ?sign ?vault_address ~nonce:(next_nonce ()) action in
  Http.post_json
    ~headers:(headers config)
    (Config.exchange_uri config)
    (Exchange_request.yojson_of_t request)
  >>| Or_error.bind ~f:decode
;;

let build_envelope ?sign ?vault_address ~nonce action =
  let unsigned =
    { Exchange_envelope.action
    ; nonce
    ; signature = None
    ; vault_address
    ; expires_after = None
    }
  in
  match sign with
  | None -> unsigned
  | Some sign ->
    { unsigned with signature = sign (Exchange_envelope.yojson_of_t unsigned) }
;;

let post_envelope config envelope =
  Http.post_json
    ~headers:(headers config)
    (Config.exchange_uri config)
    (Exchange_envelope.yojson_of_t envelope)
  >>| Or_error.bind ~f:decode
;;

let place_orders config ?sign ?vault_address ?(grouping = Grouping.Na) ?builder orders =
  match builder with
  | None -> post config ?sign ?vault_address (Order { orders; grouping })
  | Some _ ->
    let action =
      Order_action.yojson_of_t (Order_action.create ~grouping ?builder orders)
    in
    post_envelope
      config
      (build_envelope ?sign ?vault_address ~nonce:(next_nonce ()) action)
;;

let build_user_signed ~key action =
  let open Or_error.Let_syntax in
  let signature = Signing.sign_user_action key action in
  let%bind signer = Signing.recover_user_action action signature in
  let expected = Signing.Private_key.address key in
  if not (Address.equal signer expected)
  then
    Or_error.error_s
      [%message
        "user-signed action recovers to another address"
          (signer : Address.t)
          (expected : Address.t)]
  else
    return
      (User_signed_request.create
         ~signature:(Signing.Signature.yojson_of_t signature)
         action)
;;

let post_user_signed config ~key action =
  match build_user_signed ~key action with
  | Error _ as error -> return error
  | Ok request ->
    let config =
      { config with Config.address = Some (Signing.Private_key.address key) }
    in
    Http.post_json
      ~headers:(headers config)
      (Config.exchange_uri config)
      (User_signed_request.yojson_of_t request)
    >>| Or_error.bind ~f:decode
;;

(* "valid_until" may be at most 180 days ahead (exchange endpoint docs, "Approve an API
   wallet"). *)
let max_agent_validity = Time_ns.Span.of_day 180.

let agent_name ?(now = Time_ns.now ()) ?valid_until name =
  if String.is_empty (String.strip name)
  then Or_error.error_string "agent name must not be empty"
  else (
    match valid_until with
    | None -> Ok name
    | Some until ->
      let until_ms = Time_ns.to_int_ns_since_epoch until / 1_000_000 in
      if Time_ns.( <= ) until now
      then Or_error.error_s [%message "valid_until is not in the future" (until_ms : int)]
      else if Time_ns.Span.( > ) (Time_ns.diff until now) max_agent_validity
      then
        Or_error.error_s
          [%message "valid_until is more than 180 days ahead" (until_ms : int)]
      else Ok (sprintf "%s valid_until %d" name until_ms))
;;

let approve_agent
  config
  ~key
  ~hyperliquid_chain
  ?(nonce = next_nonce ())
  ?valid_until
  ~agent_address
  ~agent_name:name
  ()
  =
  match agent_name ?valid_until name with
  | Error _ as error -> return error
  | Ok agent_name ->
    post_user_signed
      config
      ~key
      (User_signed_action.approve_agent
         hyperliquid_chain
         ~agent_name
         ~agent_address
         ~nonce)
;;

let approve_builder_fee
  config
  ~key
  ~hyperliquid_chain
  ?(nonce = next_nonce ())
  ~builder
  ~max_fee_tenths_bp
  ()
  =
  if max_fee_tenths_bp < 0
  then
    return (Or_error.error_s [%message "negative builder fee" (max_fee_tenths_bp : int)])
  else
    post_user_signed
      config
      ~key
      (User_signed_action.approve_builder_fee
         hyperliquid_chain
         ~builder
         ~max_fee_tenths_bp
         ~nonce)
;;

let cancel config ?sign ?vault_address cancels =
  post config ?sign ?vault_address (Cancel { cancels })
;;

let cancel_by_cloid config ?sign ?vault_address cancels =
  post config ?sign ?vault_address (Cancel_by_cloid { cancels })
;;

let modify config ?sign ?vault_address modify =
  post config ?sign ?vault_address (Modify modify)
;;
