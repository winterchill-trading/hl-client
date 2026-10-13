open! Core
open Async
open Hl_protocol
open Hl_client

let leader = Address.of_string "0x00000000000000000000000000000000000000aa"

let fill ~time ~tid =
  sprintf
    {|{"coin":"BTC","px":"100","sz":"0.01","side":"B","time":%d,"startPosition":"0","dir":"Open Long","closedPnl":"0","hash":"0x00","oid":1,"crossed":true,"fee":"0","tid":%d,"feeToken":"USDC"}|}
    time
    tid
;;

(* The first page is full (2000 fills at times 1..2000); the second starts at 2000, so it
   repeats the fill at 2000 and adds two. *)
let respond (request : Http_capture.Request.t) =
  let start_time =
    Yojson.Safe.from_string request.body
    |> Yojson.Safe.Util.member "startTime"
    |> Yojson.Safe.Util.to_int
  in
  let fills =
    if start_time = 1
    then List.init 2000 ~f:(fun i -> fill ~time:(i + 1) ~tid:(i + 1))
    else
      [ fill ~time:2000 ~tid:2000; fill ~time:2001 ~tid:2001; fill ~time:2001 ~tid:2002 ]
  in
  "[" ^ String.concat ~sep:"," fills ^ "]"
;;

let%expect_test "userFillsByTime for another address, paged" =
  let%bind server = Http_capture.start ~respond () in
  let config = Config.create ~base_url:(Http_capture.base_url server) () in
  let%bind one = Info.user_fills_by_time ~user:leader config ~start_time:5000 in
  print_s [%sexp (Or_error.map one ~f:List.length : int Or_error.t)];
  let%bind all = Info.all_user_fills_since ~user:leader config ~start_time:1 in
  let all = Or_error.ok_exn all in
  print_s
    [%message
      ""
        ~count:(List.length all : int)
        ~last:
          (List.map
             (List.drop all (List.length all - 3))
             ~f:(fun (f : User_fill.t) -> f.time, f.tid)
           : (int * int) list)];
  List.iter (Http_capture.requests server) ~f:(fun r -> print_endline r.body);
  let%bind none = Info.user_fills_by_time config ~start_time:1 in
  print_s [%sexp (Or_error.map none ~f:List.length : int Or_error.t)];
  let%bind () = Http_capture.close server in
  [%expect
    {|
    (Ok 3)
    ((count 2002) (last ((2000 2000) (2001 2001) (2001 2002))))
    {"type":"userFillsByTime","user":"0x00000000000000000000000000000000000000aa","startTime":5000}
    {"type":"userFillsByTime","user":"0x00000000000000000000000000000000000000aa","startTime":1}
    {"type":"userFillsByTime","user":"0x00000000000000000000000000000000000000aa","startTime":2000}
    (Error "no user given and Config.address is not set")
    |}];
  return ()
;;
