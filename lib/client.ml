module Info_endpoint = Info
open! Core

type read_only
type trading

type 'access t =
  { config : Config.t
  ; key : Signing.Private_key.t option
  ; is_mainnet : bool
  }

let sexp_of_t { config; key; is_mainnet } =
  [%sexp
    { config : Config.t
    ; signed : bool = Option.is_some key
    ; network : string = (if is_mainnet then "mainnet" else "testnet")
    }]
;;

let default_is_mainnet (config : Config.t) =
  match Uri.host config.base_url with
  | Some "api.hyperliquid.xyz" -> true
  | _ -> false
;;

let read_only config = { config; key = None; is_mainnet = default_is_mainnet config }

let trading ?key ?is_mainnet (config : Config.t) =
  let config =
    match config.address, key with
    | None, Some key -> { config with address = Some (Signing.Private_key.address key) }
    | _ -> config
  in
  { config
  ; key
  ; is_mainnet =
      Option.value_or_thunk is_mainnet ~default:(fun () -> default_is_mainnet config)
  }
;;

let trading_of_env ?var ?is_mainnet config =
  Or_error.map (Signing.Private_key.of_env ?var ()) ~f:(fun key ->
    trading ~key ?is_mainnet config)
;;

let config t = t.config
let to_read_only t = { t with key = None }

let sign t =
  Option.map t.key ~f:(fun key -> Signing.sign_request key ~is_mainnet:t.is_mainnet)
;;

let l2_book ?n_sig_figs ?mantissa t coin =
  Info_endpoint.l2_book ?n_sig_figs ?mantissa t.config coin
;;

let all_mids t = Info_endpoint.all_mids t.config
let open_orders ?user t = Info_endpoint.open_orders ?user t.config
let user_fills ?user t = Info_endpoint.user_fills ?user t.config
let clearinghouse_state ?user t = Info_endpoint.clearinghouse_state ?user t.config
let meta t = Info_endpoint.meta t.config

let build_request ?vault_address t ~nonce action =
  Exchange.build_request ?sign:(sign t) ?vault_address ~nonce action
;;

let post ?vault_address t action =
  Exchange.post t.config ?sign:(sign t) ?vault_address action
;;

let place_orders ?vault_address ?grouping t orders =
  Exchange.place_orders t.config ?sign:(sign t) ?vault_address ?grouping orders
;;

let cancel ?vault_address t cancels =
  Exchange.cancel t.config ?sign:(sign t) ?vault_address cancels
;;

let cancel_by_cloid ?vault_address t cancels =
  Exchange.cancel_by_cloid t.config ?sign:(sign t) ?vault_address cancels
;;

let modify ?vault_address t modify =
  Exchange.modify t.config ?sign:(sign t) ?vault_address modify
;;
