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

let user_fills_detailed ?user config =
  with_user ?user config ~f:(fun user ->
    Http.post_json (Config.info_uri config) (request_body (User_fills { user }))
    >>| Or_error.bind
          ~f:
            (Http.decode_json
               ~f:
                 (Ppx_yojson_conv_lib.Yojson_conv.Primitives.list_of_yojson
                    User_fill.Detailed.t_of_yojson)))
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

let user_fills_by_time ?user ?end_time ?aggregate_by_time config ~start_time =
  with_user ?user config ~f:(fun user ->
    let request = { User_fills_by_time.user; start_time; end_time; aggregate_by_time } in
    Http.post_json (Config.info_uri config) (User_fills_by_time.yojson_of_t request)
    >>| Or_error.bind ~f:(Http.decode_json ~f:User_fills_by_time.response_of_yojson))
;;

let all_user_fills_since
  ?user
  ?end_time
  ?aggregate_by_time
  ?(max_pages = 5)
  config
  ~start_time
  =
  with_user ?user config ~f:(fun user ->
    let seen = Int.Hash_set.create () in
    let rec loop ~pages ~start_time acc =
      match%bind
        user_fills_by_time ~user ?end_time ?aggregate_by_time config ~start_time
      with
      | Error _ as error -> return error
      | Ok page ->
        let fresh =
          List.filter page ~f:(fun (fill : User_fill.t) ->
            if Hash_set.mem seen fill.tid
            then false
            else (
              Hash_set.add seen fill.tid;
              true))
        in
        let acc = fresh @ acc in
        let latest =
          List.fold page ~init:start_time ~f:(fun latest (fill : User_fill.t) ->
            Int.max latest fill.time)
        in
        if List.length page < User_fills_by_time.max_fills_per_response
           || List.is_empty fresh
           || pages + 1 >= max_pages
        then return (Ok acc)
        else loop ~pages:(pages + 1) ~start_time:latest acc
    in
    let%map fills = loop ~pages:0 ~start_time [] in
    Or_error.map fills ~f:(fun fills ->
      List.sort fills ~compare:(fun (a : User_fill.t) (b : User_fill.t) ->
        match Int.compare a.time b.time with
        | 0 -> Int.compare a.tid b.tid
        | c -> c)))
;;

let max_builder_fee config ~user ~builder =
  let request = { Max_builder_fee.user; builder } in
  Http.post_json (Config.info_uri config) (Max_builder_fee.yojson_of_t request)
  >>| Or_error.bind ~f:(Http.decode_json ~f:Max_builder_fee.response_of_yojson)
;;
