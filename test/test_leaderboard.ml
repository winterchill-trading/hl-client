open! Core
open Async
open Hl_client

let row address account_value day_pnl day_roi =
  sprintf
    {|{"ethAddress":"%s","accountValue":"%s","windowPerformances":[["day",{"pnl":"%s","roi":"%s","vlm":"10"}],["allTime",{"pnl":"1","roi":"0.1","vlm":"10"}]],"prize":0,"displayName":null}|}
    address
    account_value
    day_pnl
    day_roi
;;

let body =
  sprintf
    {|{"leaderboardRows":[%s,%s,%s,%s]}|}
    (row "0x00000000000000000000000000000000000000a1" "1000" "50" "0.05")
    (row "0x00000000000000000000000000000000000000a2" "100000" "900" "0.009")
    (row "0x00000000000000000000000000000000000000a3" "10" "5" "0.5")
    {|{"ethAddress":"0x00000000000000000000000000000000000000a4","accountValue":"5000","windowPerformances":[],"prize":0,"displayName":"no windows"}|}
;;

let%expect_test "fetch and rank" =
  let%bind server = Http_capture.start ~response:body () in
  let uri = Uri.with_path (Http_capture.base_url server) "/Testnet/leaderboard" in
  let%bind board = Leaderboard.fetch ~uri () in
  let board = Or_error.ok_exn board in
  let show ranked =
    List.iter ranked ~f:(fun ((row : Hl_protocol.Leaderboard.Row.t), performance) ->
      print_s
        [%sexp
          (row.eth_address : Hl_protocol.Address.t)
          , (performance : Hl_protocol.Leaderboard.Performance.t)])
  in
  show (Leaderboard.rank board ~window:Day ~by:`Roi ~limit:2);
  print_endline "--";
  show (Leaderboard.rank board ~min_account_value:100. ~window:Day ~by:`Pnl ~limit:5);
  print_s
    [%sexp (List.map (Http_capture.requests server) ~f:(fun r -> r.path) : string list)];
  let%bind () = Http_capture.close server in
  [%expect
    {|
    (0x00000000000000000000000000000000000000a3 ((pnl 5) (roi 0.5) (vlm 10)))
    (0x00000000000000000000000000000000000000a1 ((pnl 50) (roi 0.05) (vlm 10)))
    --
    (0x00000000000000000000000000000000000000a2 ((pnl 900) (roi 0.009) (vlm 10)))
    (0x00000000000000000000000000000000000000a1 ((pnl 50) (roi 0.05) (vlm 10)))
    (/Testnet/leaderboard)
    |}];
  return ()
;;
