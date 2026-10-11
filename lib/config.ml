open! Core
open Hl_protocol

module Uri = struct
  include Uri

  let sexp_of_t t = Sexp.Atom (Uri.to_string t)
end

type t =
  { base_url : Uri.t
  ; ws_url : Uri.t
  ; address : Address.t option
  }
[@@deriving sexp_of]

let ws_url_of_base_url base_url =
  let scheme =
    match Uri.scheme base_url with
    | Some "https" | Some "wss" -> "wss"
    | _ -> "ws"
  in
  Uri.with_path (Uri.with_scheme base_url (Some scheme)) "/ws"
;;

let create ?ws_url ?address ~base_url () =
  let ws_url =
    Option.value_or_thunk ws_url ~default:(fun () -> ws_url_of_base_url base_url)
  in
  { base_url; ws_url; address }
;;

let local ?address () =
  create ?address ~base_url:(Uri.of_string "http://localhost:3000") ()
;;

let mainnet ?address () =
  create ?address ~base_url:(Uri.of_string "https://api.hyperliquid.xyz") ()
;;

let testnet ?address () =
  create ?address ~base_url:(Uri.of_string "https://api.hyperliquid-testnet.xyz") ()
;;

let endpoint t path =
  let base_path = String.rstrip (Uri.path t.base_url) ~drop:(Char.equal '/') in
  Uri.with_path t.base_url (base_path ^ path)
;;

let info_uri t = endpoint t "/info"
let exchange_uri t = endpoint t "/exchange"
