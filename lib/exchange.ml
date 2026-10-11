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

let place_orders config ?sign ?vault_address ?(grouping = Grouping.Na) orders =
  post config ?sign ?vault_address (Order { orders; grouping })
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
