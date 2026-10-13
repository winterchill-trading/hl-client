(* [Frame_scan] readers: decimals agree with [Fixed_decimal.of_string] wherever the
   scanner accepts, frames are read without allocating, and odd shapes are refused. *)

open! Core
open Hl_client
module Md = Market_data

let quoted s = "\"" ^ s ^ "\""

let%expect_test "decimal" =
  List.iter
    [ "82525.0"
    ; "0.01671"
    ; "0"
    ; ".5"
    ; "5."
    ; "0.0000088036123"
    ; "00012.30"
    ; "99999999999.99999999"
    ; "-1.5"
    ; "1e5"
    ; ""
    ; "."
    ; "123456789012.0"
    ]
    ~f:(fun s ->
      print_s
        [%message
          s
            ~scan:(Frame_scan.decimal (quoted s) 0 : int)
            ~of_string:
              (Option.map (Md.Price.of_string_opt s) ~f:Md.Price.to_units : int option)]);
  [%expect
    {|
    (82525.0 (scan 8252500000000) (of_string (8252500000000)))
    (0.01671 (scan 1671000) (of_string (1671000)))
    (0 (scan 0) (of_string (0)))
    (.5 (scan 50000000) (of_string (50000000)))
    (5. (scan 500000000) (of_string (500000000)))
    (0.0000088036123 (scan 880) (of_string (880)))
    (00012.30 (scan 1230000000) (of_string (1230000000)))
    (99999999999.99999999 (scan -1) (of_string ()))
    (-1.5 (scan -1) (of_string (-150000000)))
    (1e5 (scan -1) (of_string ()))
    ("" (scan -1) (of_string ()))
    (. (scan -1) (of_string ()))
    (123456789012.0 (scan -1) (of_string ()))
    |}]
;;

let%test_unit "decimal agrees with of_string where it accepts" =
  let gen = String.gen' (Quickcheck.Generator.of_list [ '0'; '1'; '5'; '9'; '.' ]) in
  Quickcheck.test gen ~sexp_of:String.sexp_of_t ~f:(fun s ->
    let scan = Frame_scan.decimal (quoted s) 0 in
    match Md.Price.of_string_opt s with
    | Some p -> [%test_result: int] scan ~expect:(Md.Price.to_units p)
    | None -> [%test_result: int] scan ~expect:(-1))
;;

let trades_frame =
  {|{"channel":"trades","data":[{"coin":"BTC","side":"A","px":"82525.0","sz":"0.01671","time":1791812100812,"hash":"0x6b","tid":112328450464875,"users":["0x88","0xf5"]}, {"coin":"BTC","side":"B","px":"82526.5","sz":"1","time":1791812100813,"hash":"0xe9","tid":17,"users":["0x88","0xf5"]}]}|}
;;

let book_frame =
  {|{"channel":"l2Book","data":{"coin":"BTC","time":1791812103008,"levels":[[{"px":"82514.0","sz":"0.73912","n":1},{"px":"82512.0","sz":"0.7989","n":1}],[{"px":"82515.0","sz":"2.07099","n":22}]]}}|}
;;

let read_trades frame =
  let t = Frame_scan.Trade.create () in
  let rec loop p =
    if p >= 0
    then (
      let stop = Frame_scan.value_end frame p in
      if not (Frame_scan.Trade.read frame ~from:p ~stop t)
      then print_endline "refused"
      else
        print_s
          [%message
            ""
              ~coin:(String.sub frame ~pos:t.coin_pos ~len:t.coin_len : string)
              ~buy:(t.buy : bool)
              ~px:(t.px : int)
              ~sz:(t.sz : int)
              ~time:(t.time : int)
              ~tid:(t.tid : int)];
      loop (Frame_scan.next_element frame stop))
    else print_s [%message "end" ~at:(p : int)]
  in
  loop (Frame_scan.first_element frame (Frame_scan.trades_data frame))
;;

let%expect_test "trades" =
  print_s [%sexp (Frame_scan.channel trades_frame : Frame_scan.Channel.t)];
  read_trades trades_frame;
  [%expect
    {|
    Trades
    ((coin BTC) (buy false) (px 8252500000000) (sz 1671000) (time 1791812100812)
     (tid 112328450464875))
    ((coin BTC) (buy true) (px 8252650000000) (sz 100000000) (time 1791812100813)
     (tid 17))
    (end (at -1))
    |}];
  read_trades {|{"channel":"trades","data":[{"coin":"BTC","side":"X","px":"1"}]}|};
  [%expect {|
    refused
    (end (at -1))
    |}]
;;

let%expect_test "book" =
  let t = Frame_scan.Book.create () in
  let show frame =
    if Frame_scan.Book.read frame t
    then
      print_s
        [%message
          ""
            ~coin:(String.sub frame ~pos:t.coin_pos ~len:t.coin_len : string)
            ~time:(t.time : int)
            ~bids:(Array.sub t.bid_px ~pos:0 ~len:t.n_bids : int array)
            ~bid_sizes:(Array.sub t.bid_sz ~pos:0 ~len:t.n_bids : int array)
            ~asks:(Array.sub t.ask_px ~pos:0 ~len:t.n_asks : int array)]
    else print_endline "refused"
  in
  print_s [%sexp (Frame_scan.channel book_frame : Frame_scan.Channel.t)];
  show book_frame;
  show {|{"channel":"l2Book","data":{"coin":"BTC","time":1,"levels":[[],[]]}}|};
  show {|{"channel":"l2Book","data":{"coin":"BTC","time":1,"levels":[[]]}}|};
  show {|{"channel":"l2Book","data":{"coin":"B\"TC","time":1,"levels":[[],[]]}}|};
  show
    {|{"channel":"l2Book","data":{"coin":"BTC","time":1,"levels":[[{"px":"1e2","sz":"1"}],[]]}}|};
  [%expect
    {|
    L2_book
    ((coin BTC) (time 1791812103008) (bids (8251400000000 8251200000000))
     (bid_sizes (73912000 79890000)) (asks (8251500000000)))
    ((coin BTC) (time 1) (bids ()) (bid_sizes ()) (asks ()))
    refused
    refused
    refused
    |}]
;;

let words f =
  let #((), report) = Gc.For_testing.measure_allocation f in
  report.minor_words_allocated + report.major_words_allocated
;;

let%expect_test "readers allocate nothing" =
  let trade = Frame_scan.Trade.create () in
  let book = Frame_scan.Book.create () in
  let read_trade () =
    let p = Frame_scan.first_element trades_frame (Frame_scan.trades_data trades_frame) in
    let stop = Frame_scan.value_end trades_frame p in
    ignore (Frame_scan.Trade.read trades_frame ~from:p ~stop trade : bool)
  in
  let read_book () = ignore (Frame_scan.Book.read book_frame book : bool) in
  let channel () = ignore (Frame_scan.channel book_frame : Frame_scan.Channel.t) in
  print_s
    [%message
      ""
        ~trade:(words read_trade : int)
        ~book:(words read_book : int)
        ~channel:(words channel : int)];
  [%expect {| ((trade 0) (book 0) (channel 0)) |}]
;;
