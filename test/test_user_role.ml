(* [userRole] against a capturing HTTP server. *)

open! Core
open Async
open Hl_protocol
open Hl_client

let user = Address.of_string "0x0000000000000000000000000000000000000001"

let%expect_test "user_role posts the documented body and decodes each role" =
  let%bind () =
    Deferred.List.iter
      ~how:`Sequential
      [ {|{"role":"user"}|}
      ; {|{"role":"subAccount","data":{"master":"0x0b8aa35c28b7c6ab18f11dc168f437a8a69fd4f8"}}|}
      ]
      ~f:(fun response ->
        let%bind server = Http_capture.start ~response () in
        let config = Config.create ~base_url:(Http_capture.base_url server) () in
        let%bind role = Info.user_role config ~user in
        print_s [%sexp (role : User_role.Role.t Or_error.t)];
        List.iter (Http_capture.requests server) ~f:(fun r -> print_endline r.body);
        Http_capture.close server)
  in
  [%expect
    {|
    (Ok User)
    {"type":"userRole","user":"0x0000000000000000000000000000000000000001"}
    (Ok (Sub_account (master 0x0b8aa35c28b7c6ab18f11dc168f437a8a69fd4f8)))
    {"type":"userRole","user":"0x0000000000000000000000000000000000000001"}
    |}];
  return ()
;;
