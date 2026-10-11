open! Core
open Hl_protocol
open Hl_client

let order =
  { Order_request.asset = Asset.of_int 0
  ; is_buy = true
  ; limit_px = Price.of_string "113000.5"
  ; sz = Size.of_string "0.01"
  ; reduce_only = false
  ; order_type = Order_type.alo
  ; cloid = Some (Cloid.of_int 7)
  }
;;

let print_request request =
  print_endline (Yojson.Safe.to_string (Exchange_request.yojson_of_t request))
;;

let%expect_test "unsigned order request" =
  print_request
    (Exchange.build_request
       ~nonce:1700000000000
       (Order { orders = [ order ]; grouping = Na }));
  [%expect
    {| {"action":{"type":"order","orders":[{"a":0,"b":true,"p":"113000.5","s":"0.01","r":false,"t":{"limit":{"tif":"Alo"}},"c":"0x00000000000000000000000000000007"}],"grouping":"na"},"nonce":1700000000000} |}]
;;

let%expect_test "sign hook sees the unsigned body" =
  let sign json =
    print_endline ("signing: " ^ Yojson.Safe.to_string json);
    Some (`Assoc [ "r", `String "0x1"; "s", `String "0x2"; "v", `Int 27 ])
  in
  print_request
    (Exchange.build_request
       ~sign
       ~vault_address:(Address.of_string "0x00000000000000000000000000000000000000AA")
       ~nonce:1
       (Cancel { cancels = [ { asset = Asset.of_int 3; oid = Order_id.of_int 42 } ] }));
  [%expect
    {|
    signing: {"action":{"type":"cancel","cancels":[{"a":3,"o":42}]},"nonce":1,"vaultAddress":"0x00000000000000000000000000000000000000aa"}
    {"action":{"type":"cancel","cancels":[{"a":3,"o":42}]},"nonce":1,"signature":{"r":"0x1","s":"0x2","v":27},"vaultAddress":"0x00000000000000000000000000000000000000aa"}
    |}]
;;

let%expect_test "nonces strictly increase" =
  (* [List.init] calls [f] from the last index down, so build the list explicitly. *)
  let nonces = ref [] in
  for _ = 1 to 1000 do
    nonces := Exchange.next_nonce () :: !nonces
  done;
  let nonces = List.rev !nonces in
  print_s
    [%sexp
      (List.is_sorted_strictly nonces ~compare:Int.compare : bool)
      , (List.hd_exn nonces > 1_700_000_000_000 : bool)];
  [%expect {| (true true) |}]
;;

let decode ?(status = 200) body =
  print_s
    [%sexp
      (Exchange.decode { Http.Response.status; body } : Exchange_response.t Or_error.t)]
;;

let%expect_test "decode responses" =
  decode
    {|{"status":"ok","response":{"type":"order","data":{"statuses":[{"resting":{"oid":77738308}},{"error":"Order must have minimum value of $10."}]}}}|};
  decode {|{"status":"ok","response":{"type":"cancel","data":{"statuses":["success"]}}}|};
  decode {|{"status":"ok","response":{"type":"default"}}|};
  decode ~status:400 {|{"status":"err","response":"bad nonce"}|};
  decode ~status:502 "Bad Gateway";
  [%expect
    {|
    (Ok
     (Ok
      (Order
       ((Resting (oid 77738308) (cloid ()))
        (Error "Order must have minimum value of $10.")))))
    (Ok (Ok (Cancel (Success))))
    (Ok (Ok Default))
    (Ok (Err "bad nonce"))
    (Error (((status 502) (body "Bad Gateway")) "unexpected HTTP status"))
    |}]
;;
