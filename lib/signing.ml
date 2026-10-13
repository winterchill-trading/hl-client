open! Core
open Hl_protocol
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let context = lazy (Secp256k1.Context.create [ Sign; Verify ])
let to_hex s = String.concat_map s ~f:(fun c -> sprintf "%02x" (Char.to_int c))

let of_hex s =
  let s = String.chop_prefix_if_exists s ~prefix:"0x" in
  if String.length s % 2 <> 0
  then Or_error.error_string "odd number of hex digits"
  else
    Or_error.try_with (fun () ->
      String.init
        (String.length s / 2)
        ~f:(fun i ->
          Char.of_int_exn (Int.of_string ("0x" ^ String.sub s ~pos:(2 * i) ~len:2))))
;;

let address_of_public_key pk =
  let bytes =
    Bigstring.to_string (Secp256k1.Key.to_bytes ~compress:false (force context) pk)
  in
  let hash = Keccak.digest (String.drop_prefix bytes 1) in
  Address.of_string ("0x" ^ to_hex (String.drop_prefix hash 12))
;;

module Private_key = struct
  type t = Secp256k1.Key.secret Secp256k1.Key.t

  let sexp_of_t _ = Sexp.Atom "<private key>"

  let of_hex hex =
    let open Or_error.Let_syntax in
    let%bind bytes = of_hex hex in
    if String.length bytes <> 32
    then Or_error.error_string "private key must be 32 bytes"
    else
      Secp256k1.Key.read_sk (force context) (Bigstring.of_string bytes)
      |> Result.map_error ~f:Error.of_string
  ;;

  let of_env ?(var = "HL_PRIVATE_KEY") () =
    match Sys.getenv var with
    | None -> Or_error.error_s [%message "environment variable not set" var]
    | Some hex -> Or_error.tag (of_hex (String.strip hex)) ~tag:var
  ;;

  let address t = address_of_public_key (Secp256k1.Key.neuterize_exn (force context) t)
end

module Signature = struct
  type t =
    { r : string
    ; s : string
    ; v : int
    }
  [@@deriving sexp, equal, yojson]
end

let be_bytes ~len n =
  String.init len ~f:(fun i -> Char.of_int_exn ((n asr (8 * (len - 1 - i))) land 0xff))
;;

let address_bytes address = Or_error.ok_exn (of_hex (Address.to_string address))

let action_hash ?expires_after ~vault_address ~nonce action =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Msgpack.to_string (Msgpack.of_yojson action));
  Buffer.add_string buf (be_bytes ~len:8 nonce);
  (match vault_address with
   | None -> Buffer.add_char buf '\x00'
   | Some address ->
     Buffer.add_char buf '\x01';
     Buffer.add_string buf (address_bytes address));
  Option.iter expires_after ~f:(fun expires_after ->
    Buffer.add_char buf '\x00';
    Buffer.add_string buf (be_bytes ~len:8 expires_after));
  Keccak.digest (Buffer.contents buf)
;;

let word_of_int n = String.make 24 '\x00' ^ be_bytes ~len:8 n

let domain_separator =
  lazy
    (Keccak.digest
       (String.concat
          [ Keccak.digest
              "EIP712Domain(string name,string version,uint256 chainId,address \
               verifyingContract)"
          ; Keccak.digest "Exchange"
          ; Keccak.digest "1"
          ; word_of_int 1337
          ; String.make 32 '\x00'
          ]))
;;

let agent_type_hash = lazy (Keccak.digest "Agent(string source,bytes32 connectionId)")

let l1_digest ~is_mainnet ~connection_id =
  let source = if is_mainnet then "a" else "b" in
  let struct_hash =
    Keccak.digest
      (String.concat [ force agent_type_hash; Keccak.digest source; connection_id ])
  in
  Keccak.digest (String.concat [ "\x19\x01"; force domain_separator; struct_hash ])
;;

let minimal_hex bytes =
  match String.lstrip (to_hex bytes) ~drop:(Char.equal '0') with
  | "" -> "0x0"
  | digits -> "0x" ^ digits
;;

let sign_digest key digest =
  let ctx = force context in
  let msg = Secp256k1.Sign.msg_of_bytes_exn (Bigstring.of_string digest) in
  let signature = Secp256k1.Sign.sign_recoverable_exn ctx ~sk:key msg in
  let bytes, recid = Secp256k1.Sign.to_bytes_recid ctx signature in
  let bytes = Bigstring.to_string bytes in
  { Signature.r = minimal_hex (String.prefix bytes 32)
  ; s = minimal_hex (String.sub bytes ~pos:32 ~len:32)
  ; v = 27 + recid
  }
;;

let sign_l1_action ?expires_after key ~is_mainnet ~vault_address ~nonce action =
  let connection_id = action_hash ?expires_after ~vault_address ~nonce action in
  sign_digest key (l1_digest ~is_mainnet ~connection_id)
;;

let word_of_hex hex =
  let open Or_error.Let_syntax in
  let hex = String.chop_prefix_if_exists hex ~prefix:"0x" in
  let%bind bytes = of_hex (if String.length hex % 2 = 0 then hex else "0" ^ hex) in
  if String.length bytes > 32
  then Or_error.error_string "signature component longer than 32 bytes"
  else return (String.make (32 - String.length bytes) '\x00' ^ bytes)
;;

let recover ~digest (signature : Signature.t) =
  let open Or_error.Let_syntax in
  let ctx = force context in
  let%bind r = word_of_hex signature.r in
  let%bind s = word_of_hex signature.s in
  let%bind recoverable =
    Secp256k1.Sign.read_recoverable
      ctx
      ~recid:(signature.v - 27)
      (Bigstring.of_string (r ^ s))
    |> Result.map_error ~f:Error.of_string
  in
  let msg = Secp256k1.Sign.msg_of_bytes_exn (Bigstring.of_string digest) in
  let%map pk =
    Secp256k1.Sign.recover ctx ~signature:recoverable ~msg
    |> Result.map_error ~f:Error.of_string
  in
  address_of_public_key pk
;;

(* EIP-712 encoding of the [HyperliquidSignTransaction] domain and its messages, as in the
   SDK's [user_signed_payload]: atomic values are 32-byte words, strings are hashed. *)
let user_signed_domain_separator ~chain_id =
  Keccak.digest
    (String.concat
       [ Keccak.digest
           "EIP712Domain(string name,string version,uint256 chainId,address \
            verifyingContract)"
       ; Keccak.digest User_signed_action.Eip712.domain_name
       ; Keccak.digest User_signed_action.Eip712.domain_version
       ; word_of_int chain_id
       ; String.make 32 '\x00'
       ])
;;

let encode_value : User_signed_action.Eip712.Value.t -> string = function
  | String s -> Keccak.digest s
  | Address address -> String.make 12 '\x00' ^ address_bytes address
  | Uint64 n -> word_of_int n
;;

let user_signed_digest ~chain_id ~primary_type fields =
  let encode_type =
    primary_type
    ^ "("
    ^ String.concat
        ~sep:","
        (List.map fields ~f:(fun (name, value) ->
           User_signed_action.Eip712.Value.solidity_type value ^ " " ^ name))
    ^ ")"
  in
  let struct_hash =
    Keccak.digest
      (String.concat
         (Keccak.digest encode_type
          :: List.map fields ~f:(fun (_, value) -> encode_value value)))
  in
  Keccak.digest
    (String.concat [ "\x19\x01"; user_signed_domain_separator ~chain_id; struct_hash ])
;;

let user_action_digest action =
  user_signed_digest
    ~chain_id:(User_signed_action.Eip712.chain_id action)
    ~primary_type:(User_signed_action.Eip712.primary_type action)
    (User_signed_action.Eip712.fields action)
;;

let sign_user_action key action = sign_digest key (user_action_digest action)

let recover_user_action action signature =
  recover ~digest:(user_action_digest action) signature
;;

let sign_request key ~is_mainnet request =
  let action = Json.member_exn request ~key:"action" in
  let nonce =
    match Json.member_exn request ~key:"nonce" with
    | `Int nonce -> nonce
    | json -> Json.error "Signing.sign_request: nonce must be an integer" json
  in
  let vault_address =
    match Json.member request ~key:"vaultAddress" with
    | None | Some `Null -> None
    | Some json -> Some (Address.t_of_yojson json)
  in
  Some
    (Signature.yojson_of_t (sign_l1_action key ~is_mainnet ~vault_address ~nonce action))
;;
