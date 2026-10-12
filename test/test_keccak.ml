open! Core

let%expect_test "standard vectors" =
  List.iter
    [ ""; "abc"; String.make 135 'a'; String.make 136 'a'; String.make 200 'a' ]
    ~f:(fun s -> print_endline (Hl_client.Keccak.digest_hex s));
  [%expect
    {|
    c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
    4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45
    34367dc248bbd832f4e3e69dfaac2f92638bd0bbd18f2912ba4ef454919cf446
    a6c4d403279fe3e0af03729caada8374b5ca54d8065329a3ebcaeb4b60aa386e
    96ea54061def936c4be90b518992fdc6f12f535068a256229aca54267b4d084d
    |}]
;;
