open! Core
open Async
open Hl_protocol

let request_body = Info_request.yojson_of_t

let decode request response =
  Http.decode_json response ~f:(fun json -> Info_response.of_yojson json ~request)
;;

let post config request =
  Http.post_json (Config.info_uri config) (request_body request)
  >>| Or_error.bind ~f:(decode request)
;;

let unexpected response =
  Or_error.error_s
    [%message "unexpected /info response variant" (response : Info_response.t)]
;;

let user_or_default ?user (config : Config.t) =
  match Option.first_some user config.address with
  | Some user -> Ok user
  | None -> Or_error.error_string "no user given and Config.address is not set"
;;

let with_user ?user config ~f =
  match user_or_default ?user config with
  | Error _ as e -> return e
  | Ok user -> f user
;;

let l2_book ?n_sig_figs ?mantissa config coin =
  match%map post config (L2_book { coin; n_sig_figs; mantissa }) with
  | Ok (L2_book book) -> Ok book
  | Ok other -> unexpected other
  | Error _ as e -> e
;;

let all_mids config =
  match%map post config All_mids with
  | Ok (All_mids mids) -> Ok mids
  | Ok other -> unexpected other
  | Error _ as e -> e
;;

let open_orders ?user config =
  with_user ?user config ~f:(fun user ->
    match%map post config (Open_orders { user }) with
    | Ok (Open_orders orders) -> Ok orders
    | Ok other -> unexpected other
    | Error _ as e -> e)
;;

let user_fills ?user config =
  with_user ?user config ~f:(fun user ->
    match%map post config (User_fills { user }) with
    | Ok (User_fills fills) -> Ok fills
    | Ok other -> unexpected other
    | Error _ as e -> e)
;;

let clearinghouse_state ?user config =
  with_user ?user config ~f:(fun user ->
    match%map post config (Clearinghouse_state { user }) with
    | Ok (Clearinghouse_state state) -> Ok state
    | Ok other -> unexpected other
    | Error _ as e -> e)
;;

let meta config =
  match%map post config Meta with
  | Ok (Meta meta) -> Ok meta
  | Ok other -> unexpected other
  | Error _ as e -> e
;;
