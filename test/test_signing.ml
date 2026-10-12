(* Vectors from the official Python SDK, tests/signing_test.py. *)

open! Core
open Hl_protocol
module Signing = Hl_client.Signing

let key =
  Signing.Private_key.of_hex
    "0x0123456789012345678901234567890123456789012345678901234567890123"
  |> Or_error.ok_exn
;;

let hex s = "0x" ^ String.concat_map s ~f:(fun c -> sprintf "%02x" (Char.to_int c))

let sign ?vault_address action =
  List.iter [ true; false ] ~f:(fun is_mainnet ->
    let signature =
      Signing.sign_l1_action key ~is_mainnet ~vault_address ~nonce:0 action
    in
    print_s [%sexp (is_mainnet : bool), (signature : Signing.Signature.t)])
;;

let order ?cloid ?(asset = 1) ?(px = "100") ?(sz = "100") order_type =
  Exchange_action.yojson_of_t
    (Order
       { orders =
           [ { asset = Asset.of_int asset
             ; is_buy = true
             ; limit_px = Price.of_string px
             ; sz = Size.of_string sz
             ; reduce_only = false
             ; order_type
             ; cloid = Option.map cloid ~f:Cloid.of_string
             }
           ]
       ; grouping = Na
       })
;;

let%expect_test "address and key sexp" =
  print_s
    [%sexp (key : Signing.Private_key.t), (Signing.Private_key.address key : Address.t)];
  [%expect {| ("<private key>" 0x14791697260e4c9a71f18484c9f997b308e59325) |}]
;;

let%expect_test "phantom agent matches production" =
  let action = order ~asset:4 ~px:"1670.1" ~sz:"0.0147" Order_type.ioc in
  print_endline (Yojson.Safe.to_string action);
  print_endline
    (hex (Signing.action_hash ~vault_address:None ~nonce:1677777606040 action));
  [%expect
    {|
    {"type":"order","orders":[{"a":4,"b":true,"p":"1670.1","s":"0.0147","r":false,"t":{"limit":{"tif":"Ioc"}}}],"grouping":"na"}
    0x0fcbeda5ae3c4950a548021552a4fea2226858c4453571bf3f24ba017eac2908
    |}]
;;

let%expect_test "dummy action" =
  sign (`Assoc [ "type", `String "dummy"; "num", `Int 100_000_000_000 ]);
  [%expect
    {|
    (true
     ((r 0x53749d5b30552aeb2fca34b530185976545bb22d0b3ce6f62e31be961a59298)
      (s 0x755c40ba9bf05223521753995abb2f73ab3229be8ec921f350cb447e384d8ed8)
      (v 27)))
    (false
     ((r 0x542af61ef1f429707e3c76c5293c80d01f74ef853e34b76efffcb57e574f9510)
      (s 0x17b8b32f086e8cdede991f1e2c529f5dd5297cbe8128500e00cbaf766204a613)
      (v 28)))
    |}]
;;

let%expect_test "dummy action with vault" =
  sign
    ~vault_address:(Address.of_string "0x1719884eb866cb12b2287399b15f7db5e7d775ea")
    (`Assoc [ "type", `String "dummy"; "num", `Int 100_000_000_000 ]);
  [%expect
    {|
    (true
     ((r 0x3c548db75e479f8012acf3000ca3a6b05606bc2ec0c29c50c515066a326239)
      (s 0x4d402be7396ce74fbba3795769cda45aec00dc3125a984f2a9f23177b190da2c)
      (v 28)))
    (false
     ((r 0xe281d2fb5c6e25ca01601f878e4d69c965bb598b88fac58e475dd1f5e56c362b)
      (s 0x7ddad27e9a238d045c035bc606349d075d5c5cd00a6cd1da23ab5c39d4ef0f60)
      (v 27)))
    |}]
;;

let%expect_test "order" =
  sign (order Order_type.gtc);
  [%expect
    {|
    (true
     ((r 0xd65369825a9df5d80099e513cce430311d7d26ddf477f5b3a33d2806b100d78e)
      (s 0x2b54116ff64054968aa237c20ca9ff68000f977c93289157748a3162b6ea940e)
      (v 28)))
    (false
     ((r 0x82b2ba28e76b3d761093aaded1b1cdad4960b3af30212b343fb2e6cdfa4e3d54)
      (s 0x6b53878fc99d26047f4d7e8c90eb98955a109f44209163f52d8dc4278cbbd9f5)
      (v 27)))
    |}]
;;

let%expect_test "order with cloid" =
  sign (order ~cloid:"0x00000000000000000000000000000001" Order_type.gtc);
  [%expect
    {|
    (true
     ((r 0x41ae18e8239a56cacbc5dad94d45d0b747e5da11ad564077fcac71277a946e3)
      (s 0x3c61f667e747404fe7eea8f90ab0e76cc12ce60270438b2058324681a00116da)
      (v 27)))
    (false
     ((r 0xeba0664bed2676fc4e5a743bf89e5c7501aa6d870bdb9446e122c9466c5cd16d)
      (s 0x7f3e74825c9114bc59086f1eebea2928c190fdfbfde144827cb02b85bbe90988)
      (v 28)))
    |}]
;;

let%expect_test "tpsl order" =
  sign
    (order (Trigger { is_market = true; trigger_px = Price.of_string "103"; tpsl = Sl }));
  [%expect
    {|
    (true
     ((r 0x98343f2b5ae8e26bb2587daad3863bc70d8792b09af1841b6fdd530a2065a3f9)
      (s 0x6b5bb6bb0633b710aa22b721dd9dee6d083646a5f8e581a20b545be6c1feb405)
      (v 27)))
    (false
     ((r 0x971c554d917c44e0e1b6cc45d8f9404f32172a9d3b3566262347d0302896a2e4)
      (s 0x206257b104788f80450f8e786c329daa589aa0b32ba96948201ae556d5637eac)
      (v 28)))
    |}]
;;

let%expect_test "createSubAccount and scheduleCancel" =
  sign (`Assoc [ "type", `String "createSubAccount"; "name", `String "example" ]);
  sign (`Assoc [ "type", `String "scheduleCancel" ]);
  sign (`Assoc [ "type", `String "scheduleCancel"; "time", `Int 123456789 ]);
  [%expect
    {|
    (true
     ((r 0x51096fe3239421d16b671e192f574ae24ae14329099b6db28e479b86cdd6caa7)
      (s 0xb71f7d293af92d3772572afb8b102d167a7cef7473388286bc01f52a5c5b423)
      (v 27)))
    (false
     ((r 0xa699e3ed5c2b89628c746d3298b5dc1cca604694c2c855da8bb8250ec8014a5b)
      (s 0x53f1b8153a301c72ecc655b1c315d64e1dcea3ee58921fd7507e35818fcc1584)
      (v 28)))
    (true
     ((r 0x6cdfb286702f5917e76cd9b3b8bf678fcc49aec194c02a73e6d4f16891195df9)
      (s 0x6557ac307fa05d25b8d61f21fb8a938e703b3d9bf575f6717ba21ec61261b2a0)
      (v 27)))
    (false
     ((r 0xc75bb195c3f6a4e06b7d395acc20bbb224f6d23ccff7c6a26d327304e6efaeed)
      (s 0x342f8ede109a29f2c0723bd5efb9e9100e3bbb493f8fb5164ee3d385908233df)
      (v 28)))
    (true
     ((r 0x609cb20c737945d070716dcc696ba030e9976fcf5edad87afa7d877493109d55)
      (s 0x16c685d63b5c7a04512d73f183b3d7a00da5406ff1f8aad33f8ae2163bab758b)
      (v 28)))
    (false
     ((r 0x4e4f2dbd4107c69783e251b7e1057d9f2b9d11cee213441ccfa2be63516dc5bc)
      (s 0x706c656b23428c8ba356d68db207e11139ede1670481a9e01ae2dfcdb0e1a678)
      (v 27)))
    |}]
;;

let%expect_test "recover gives back the signer" =
  let action = order Order_type.gtc in
  let signature =
    Signing.sign_l1_action key ~is_mainnet:false ~vault_address:None ~nonce:7 action
  in
  let digest =
    Signing.l1_digest
      ~is_mainnet:false
      ~connection_id:(Signing.action_hash ~vault_address:None ~nonce:7 action)
  in
  print_s [%sexp (Signing.recover ~digest signature : Address.t Or_error.t)];
  [%expect {| (Ok 0x14791697260e4c9a71f18484c9f997b308e59325) |}]
;;

let%expect_test "sign_request reads action, nonce and vault from the body" =
  let request =
    Hl_client.Exchange.build_request
      ~sign:(Signing.sign_request key ~is_mainnet:true)
      ~nonce:0
      (Cancel { cancels = [] })
  in
  print_s [%sexp (request.signature : Json.t option)];
  sign (Exchange_action.yojson_of_t (Cancel { cancels = [] }));
  [%expect
    {|
    ("{\"r\":\"0x1502f1ba61204fa1264eac05c61f8c5bd31153ffafb57f1dc5cc3ba192b92083\",\"s\":\"0xb3fc36f30e572f268c88bd6ffd7ba062935d0590dc231110e5e83592e5ace07\",\"v\":27}")
    (true
     ((r 0x1502f1ba61204fa1264eac05c61f8c5bd31153ffafb57f1dc5cc3ba192b92083)
      (s 0xb3fc36f30e572f268c88bd6ffd7ba062935d0590dc231110e5e83592e5ace07)
      (v 27)))
    (false
     ((r 0x87477b2eb7ec3899578e393ed07c85ee15f2fabd495187f23d5b8523d019a7c1)
      (s 0x25f6ca38f87ca865b2eef3684a36d2a98ab8debe0aedda31202897b15d0c871f)
      (v 28)))
    |}]
;;

let%expect_test "bad keys" =
  List.iter
    [ ""; "0x12"; String.make 64 'z'; String.make 64 '0' ]
    ~f:(fun hex ->
      print_s [%sexp (Or_error.is_ok (Signing.Private_key.of_hex hex) : bool)]);
  [%expect {|
    false
    false
    false
    false
    |}]
;;
