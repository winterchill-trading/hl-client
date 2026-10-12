open! Core
module Msgpack = Hl_client.Msgpack

let hex s = String.concat_map s ~f:(fun c -> sprintf "%02x" (Char.to_int c))
let show t = print_endline (hex (Msgpack.to_string t))

let%expect_test "integers use the smallest form" =
  List.iter
    [ 0
    ; 127
    ; 128
    ; 255
    ; 256
    ; 65535
    ; 65536
    ; 4294967295
    ; 4294967296
    ; 100_000_000_000
    ; -1
    ; -32
    ; -33
    ; -128
    ; -129
    ; -32768
    ; -32769
    ; -2147483648
    ; -2147483649
    ]
    ~f:(fun i -> show (Int i));
  [%expect
    {|
    00
    7f
    cc80
    ccff
    cd0100
    cdffff
    ce00010000
    ceffffffff
    cf0000000100000000
    cf000000174876e800
    ff
    e0
    d0df
    d080
    d1ff7f
    d18000
    d2ffff7fff
    d280000000
    d3ffffffff7fffffff
    |}]
;;

let%expect_test "strings, arrays, maps, scalars" =
  let open Msgpack in
  show (String "");
  show (String (String.make 31 'x'));
  show (String (String.make 32 'x'));
  show (Array [ Nil; Bool true; Bool false ]);
  show (Array (List.init 16 ~f:(fun _ -> Nil)));
  show (Map [ "type", String "dummy"; "num", Int 100_000_000_000 ]);
  show (Float 1.5);
  show (of_yojson (`Assoc [ "a", `List [ `Int 1; `String "b" ] ]));
  [%expect
    {|
    a0
    bf78787878787878787878787878787878787878787878787878787878787878
    d9207878787878787878787878787878787878787878787878787878787878787878
    93c0c3c2
    dc0010c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0
    82a474797065a564756d6d79a36e756dcf000000174876e800
    cb3ff8000000000000
    81a1619201a162
    |}]
;;
