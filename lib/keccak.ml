open! Core

let round_constants =
  [| 0x0000000000000001L
   ; 0x0000000000008082L
   ; 0x800000000000808aL
   ; 0x8000000080008000L
   ; 0x000000000000808bL
   ; 0x0000000080000001L
   ; 0x8000000080008081L
   ; 0x8000000000008009L
   ; 0x000000000000008aL
   ; 0x0000000000000088L
   ; 0x0000000080008009L
   ; 0x000000008000000aL
   ; 0x000000008000808bL
   ; 0x800000000000008bL
   ; 0x8000000000008089L
   ; 0x8000000000008003L
   ; 0x8000000000008002L
   ; 0x8000000000000080L
   ; 0x000000000000800aL
   ; 0x800000008000000aL
   ; 0x8000000080008081L
   ; 0x8000000000008080L
   ; 0x0000000080000001L
   ; 0x8000000080008008L
  |]
;;

let rotations =
  [| 1
   ; 3
   ; 6
   ; 10
   ; 15
   ; 21
   ; 28
   ; 36
   ; 45
   ; 55
   ; 2
   ; 14
   ; 27
   ; 41
   ; 56
   ; 8
   ; 25
   ; 43
   ; 62
   ; 18
   ; 39
   ; 61
   ; 20
   ; 44
  |]
;;

let pi_lanes =
  [| 10
   ; 7
   ; 11
   ; 17
   ; 18
   ; 3
   ; 5
   ; 16
   ; 8
   ; 21
   ; 24
   ; 4
   ; 15
   ; 23
   ; 19
   ; 13
   ; 12
   ; 2
   ; 20
   ; 14
   ; 22
   ; 9
   ; 6
   ; 1
  |]
;;

let ( ^^ ) = Int64.bit_xor
let rotl x n = Int64.bit_or (Int64.shift_left x n) (Int64.shift_right_logical x (64 - n))

let keccak_f (st : int64 array) =
  let bc = Array.create ~len:5 0L in
  for round = 0 to 23 do
    (* theta *)
    for i = 0 to 4 do
      bc.(i) <- st.(i) ^^ st.(i + 5) ^^ st.(i + 10) ^^ st.(i + 15) ^^ st.(i + 20)
    done;
    for i = 0 to 4 do
      let t = bc.((i + 4) % 5) ^^ rotl bc.((i + 1) % 5) 1 in
      for j = 0 to 4 do
        st.((5 * j) + i) <- st.((5 * j) + i) ^^ t
      done
    done;
    (* rho and pi *)
    let t = ref st.(1) in
    for i = 0 to 23 do
      let j = pi_lanes.(i) in
      let next = st.(j) in
      st.(j) <- rotl !t rotations.(i);
      t := next
    done;
    (* chi *)
    for j = 0 to 4 do
      for i = 0 to 4 do
        bc.(i) <- st.((5 * j) + i)
      done;
      for i = 0 to 4 do
        st.((5 * j) + i)
        <- st.((5 * j) + i)
           ^^ Int64.bit_and (Int64.bit_not bc.((i + 1) % 5)) bc.((i + 2) % 5)
      done
    done;
    (* iota *)
    st.(0) <- st.(0) ^^ round_constants.(round)
  done
;;

(* 1600 - 2 * 256 bits *)
let rate = 136

let digest input =
  let st = Array.create ~len:25 0L in
  let len = String.length input in
  let padded_len = ((len / rate) + 1) * rate in
  let buf = Bytes.make padded_len '\000' in
  Bytes.From_string.blit ~src:input ~src_pos:0 ~dst:buf ~dst_pos:0 ~len;
  Bytes.set buf len (Char.of_int_exn 0x01);
  let last = padded_len - 1 in
  Bytes.set buf last (Char.of_int_exn (Char.to_int (Bytes.get buf last) lor 0x80));
  for block = 0 to (padded_len / rate) - 1 do
    for lane = 0 to (rate / 8) - 1 do
      let word = Stdlib.Bytes.get_int64_le buf ((block * rate) + (lane * 8)) in
      st.(lane) <- st.(lane) ^^ word
    done;
    keccak_f st
  done;
  let out = Bytes.create 32 in
  for lane = 0 to 3 do
    Stdlib.Bytes.set_int64_le out (lane * 8) st.(lane)
  done;
  Bytes.to_string out
;;

let digest_hex input =
  String.concat_map (digest input) ~f:(fun c -> sprintf "%02x" (Char.to_int c))
;;
