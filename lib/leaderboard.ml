open! Core
open Async
open Hl_protocol

let fetch ?(network = `Mainnet) ?uri () =
  let uri = Option.value uri ~default:(Uri.of_string (Leaderboard.url network)) in
  Http.get uri >>| Or_error.bind ~f:(Http.decode_json ~f:Leaderboard.t_of_yojson)
;;

let rank ?(min_account_value = 0.) (t : Leaderboard.t) ~window ~by ~limit =
  let key (p : Leaderboard.Performance.t) =
    match by with
    | `Pnl -> p.pnl
    | `Roi -> p.roi
    | `Volume -> p.vlm
  in
  List.filter_map t.leaderboard_rows ~f:(fun row ->
    if Float.( < ) row.account_value min_account_value
    then None
    else
      Option.map (Leaderboard.Row.performance row window) ~f:(fun performance ->
        row, performance))
  |> List.stable_sort ~compare:(fun (_, a) (_, b) -> Float.descending (key a) (key b))
  |> Fn.flip List.take limit
;;
