(* [maxBuilderFee] and detailed fills against a capturing HTTP server. *)

open! Core
open Async
open Hl_protocol
open Hl_client

let user = Address.of_string "0x0000000000000000000000000000000000000001"
let builder = Address.of_string "0x00000000000000000000000000000000000000b1"

let%expect_test "max_builder_fee posts the documented body and reads the integer" =
  let%bind server = Http_capture.start ~response:"10" () in
  let config = Config.create ~base_url:(Http_capture.base_url server) () in
  let%bind fee = Info.max_builder_fee config ~user ~builder in
  print_s [%sexp (fee : int Or_error.t)];
  List.iter (Http_capture.requests server) ~f:(fun r -> print_endline r.body);
  let%bind () = Http_capture.close server in
  [%expect
    {|
    (Ok 10)
    {"type":"maxBuilderFee","user":"0x0000000000000000000000000000000000000001","builder":"0x00000000000000000000000000000000000000b1"}
    |}];
  return ()
;;

let%expect_test "user_fills_detailed keeps builderFee and liquidation" =
  let response = In_channel.read_all "fixtures/docs-user-fills/user_fills.json" in
  let%bind server = Http_capture.start ~response () in
  let config = Config.create ~base_url:(Http_capture.base_url server) () in
  let%bind fills = Info.user_fills_detailed ~user config in
  List.iter (Or_error.ok_exn fills) ~f:(fun (d : User_fill.Detailed.t) ->
    print_s
      [%message
        ""
          ~tid:(d.fill.tid : int)
          ~builder_fee:(d.builder_fee : Usd.t option)
          ~liquidation:(d.liquidation : User_fill.Liquidation.t option)]);
  (* The plain decoder still reads the same body. *)
  let%bind plain = Info.user_fills ~user config in
  print_s [%sexp (Or_error.map plain ~f:List.length : int Or_error.t)];
  let%bind () = Http_capture.close server in
  [%expect
    {|
    ((tid 118906512037719) (builder_fee (0.01)) (liquidation ()))
    ((tid 118906512037720) (builder_fee ())
     (liquidation
      (((liquidated_user (0x00000000000000000000000000000000000000aa))
        (mark_px 2399.8) (method_ market)))))
    ((tid 118906512037721) (builder_fee ())
     (liquidation (((liquidated_user ()) (mark_px 99990.5) (method_ backstop)))))
    (Ok 3)
    |}];
  return ()
;;
