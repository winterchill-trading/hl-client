open! Core

type t =
  | Nil
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Map of (string * t) list
[@@deriving sexp_of]

let rec of_yojson : Yojson.Safe.t -> t = function
  | `Null -> Nil
  | `Bool b -> Bool b
  | `Int i -> Int i
  | `Intlit s -> Int (Int.of_string s)
  | `Float f -> Float f
  | `String s -> String s
  | `List l | `Tuple l -> Array (List.map l ~f:of_yojson)
  | `Assoc kvs -> Map (List.map kvs ~f:(fun (k, v) -> k, of_yojson v))
  | `Variant _ as json ->
    raise_s [%message "Msgpack.of_yojson: variant" ~json:(Yojson.Safe.to_string json)]
;;

let add_byte buf n = Buffer.add_char buf (Char.of_int_exn (n land 0xff))

let add_be buf ~bytes n =
  for i = bytes - 1 downto 0 do
    add_byte buf (n asr (8 * i))
  done
;;

let add_header buf ~fix ~fix_max ~codes n =
  if n <= fix_max
  then add_byte buf (fix lor n)
  else (
    let c8, c16, c32 = codes in
    match c8 with
    | Some c8 when n < 0x100 ->
      add_byte buf c8;
      add_be buf ~bytes:1 n
    | _ when n < 0x10000 ->
      add_byte buf c16;
      add_be buf ~bytes:2 n
    | _ ->
      add_byte buf c32;
      add_be buf ~bytes:4 n)
;;

let add_int buf i =
  if i >= 0
  then
    if i < 0x80
    then add_byte buf i
    else if i < 0x100
    then (
      add_byte buf 0xcc;
      add_be buf ~bytes:1 i)
    else if i < 0x10000
    then (
      add_byte buf 0xcd;
      add_be buf ~bytes:2 i)
    else if i < 0x1_0000_0000
    then (
      add_byte buf 0xce;
      add_be buf ~bytes:4 i)
    else (
      add_byte buf 0xcf;
      add_be buf ~bytes:8 i)
  else if i >= -32
  then add_byte buf (0xe0 lor (i land 0x1f))
  else if i >= -0x80
  then (
    add_byte buf 0xd0;
    add_be buf ~bytes:1 i)
  else if i >= -0x8000
  then (
    add_byte buf 0xd1;
    add_be buf ~bytes:2 i)
  else if i >= -0x8000_0000
  then (
    add_byte buf 0xd2;
    add_be buf ~bytes:4 i)
  else (
    add_byte buf 0xd3;
    add_be buf ~bytes:8 i)
;;

let add_string buf s =
  add_header buf ~fix:0xa0 ~fix_max:31 ~codes:(Some 0xd9, 0xda, 0xdb) (String.length s);
  Buffer.add_string buf s
;;

let rec add buf = function
  | Nil -> add_byte buf 0xc0
  | Bool false -> add_byte buf 0xc2
  | Bool true -> add_byte buf 0xc3
  | Int i -> add_int buf i
  | Float f ->
    add_byte buf 0xcb;
    let bits = Int64.bits_of_float f in
    for i = 7 downto 0 do
      add_byte buf (Int64.to_int_trunc (Int64.shift_right_logical bits (8 * i)))
    done
  | String s -> add_string buf s
  | Array items ->
    add_header buf ~fix:0x90 ~fix_max:15 ~codes:(None, 0xdc, 0xdd) (List.length items);
    List.iter items ~f:(add buf)
  | Map kvs ->
    add_header buf ~fix:0x80 ~fix_max:15 ~codes:(None, 0xde, 0xdf) (List.length kvs);
    List.iter kvs ~f:(fun (k, v) ->
      add_string buf k;
      add buf v)
;;

let to_string t =
  let buf = Buffer.create 64 in
  add buf t;
  Buffer.contents buf
;;
