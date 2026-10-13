open! Core

module Channel = struct
  type t =
    | Trades
    | L2_book
    | Active_asset_ctx
    | Other
  [@@deriving sexp_of]
end

let[@zero_alloc] is_ws c =
  Char.equal c ' ' || Char.equal c '\n' || Char.equal c '\r' || Char.equal c '\t'
;;

let[@zero_alloc] skip_ws s pos =
  let n = String.length s in
  let mutable i = pos in
  while i < n && is_ws (String.unsafe_get s i) do
    i <- i + 1
  done;
  i
;;

let[@zero_alloc] equal_sub s ~pos ~len name =
  len = String.length name
  && pos >= 0
  && pos + len <= String.length s
  &&
  let mutable j = 0 in
  while
    j < len && Char.equal (String.unsafe_get s (pos + j)) (String.unsafe_get name j)
  do
    j <- j + 1
  done;
  j = len
;;

(* Position just after [key] in [\[from, stop)], or -1. *)
let[@zero_alloc] find_raw s ~from ~stop key =
  let k = String.length key in
  let stop = Int.min stop (String.length s) in
  let first = String.unsafe_get key 0 in
  let mutable i = from in
  let mutable found = -1 in
  while found < 0 && i + k <= stop do
    if Char.equal (String.unsafe_get s i) first && equal_sub s ~pos:i ~len:k key
    then found <- i + k
    else i <- i + 1
  done;
  found
;;

let[@zero_alloc] find_key s ~from ~stop key =
  let n = String.length s in
  let mutable from = from in
  let mutable result = -2 in
  while result = -2 do
    let after = find_raw s ~from ~stop key in
    if after < 0
    then result <- -1
    else (
      let colon = skip_ws s after in
      if colon < n && Char.equal (String.unsafe_get s colon) ':'
      then result <- skip_ws s (colon + 1)
      else (* The key's text appeared as a value; keep looking. *)
        from <- after)
  done;
  result
;;

(* Position of the closing quote of the string opening at [pos], or -1 (unterminated or
   escaped). *)
let[@zero_alloc] string_close s pos =
  let n = String.length s in
  if pos >= n || not (Char.equal (String.unsafe_get s pos) '"')
  then -1
  else (
    let mutable i = pos + 1 in
    let mutable result = -2 in
    while result = -2 do
      if i >= n
      then result <- -1
      else (
        let c = String.unsafe_get s i in
        if Char.equal c '"'
        then result <- i
        else if Char.equal c '\\'
        then result <- -1
        else i <- i + 1)
    done;
    result)
;;

let[@zero_alloc] value_end s pos =
  let n = String.length s in
  let mutable i = pos in
  let mutable depth = 0 in
  let mutable in_string = false in
  let mutable result = -2 in
  while result = -2 do
    if i >= n
    then result <- -1
    else (
      let c = String.unsafe_get s i in
      if in_string
      then
        if Char.equal c '\\'
        then i <- i + 2
        else (
          if Char.equal c '"' then in_string <- false;
          i <- i + 1)
      else (
        if Char.equal c '"'
        then in_string <- true
        else if Char.equal c '{' || Char.equal c '['
        then depth <- depth + 1
        else if Char.equal c '}' || Char.equal c ']'
        then (
          depth <- depth - 1;
          if depth = 0 then result <- i + 1);
        i <- i + 1))
  done;
  result
;;

let units_per_one = 100_000_000
let decimals = 8

let[@zero_alloc] decimal s pos =
  let n = String.length s in
  if pos < 0 || pos >= n || not (Char.equal (String.unsafe_get s pos) '"')
  then -1
  else (
    let mutable i = pos + 1 in
    let mutable whole = 0 in
    let mutable int_digits = 0 in
    let mutable significant = 0 in
    while i < n && Char.is_digit (String.unsafe_get s i) do
      let d = Char.to_int (String.unsafe_get s i) - 48 in
      if significant > 0 || d > 0 then significant <- significant + 1;
      (* Past 11 significant digits the value is out of range anyway; stop accumulating so
         [whole] cannot overflow. *)
      if significant <= 11 then whole <- (whole * 10) + d;
      int_digits <- int_digits + 1;
      i <- i + 1
    done;
    let mutable frac = 0 in
    let mutable frac_digits = 0 in
    let mutable has_point = false in
    if i < n && Char.equal (String.unsafe_get s i) '.'
    then (
      has_point <- true;
      i <- i + 1;
      while i < n && Char.is_digit (String.unsafe_get s i) do
        if frac_digits < decimals
        then frac <- (frac * 10) + (Char.to_int (String.unsafe_get s i) - 48);
        frac_digits <- frac_digits + 1;
        i <- i + 1
      done);
    ignore has_point;
    if i >= n
       || (not (Char.equal (String.unsafe_get s i) '"'))
       || (int_digits = 0 && frac_digits = 0)
       || significant > 11
    then -1
    else (
      let mutable f = Int.min frac_digits decimals in
      while f < decimals do
        frac <- frac * 10;
        f <- f + 1
      done;
      if whole > (Int.max_value - frac) / units_per_one
      then -1
      else (whole * units_per_one) + frac))
;;

let[@zero_alloc] int_at s pos =
  let n = String.length s in
  if pos < 0 || pos >= n || not (Char.is_digit (String.unsafe_get s pos))
  then -1
  else (
    let mutable i = pos in
    let mutable v = 0 in
    let mutable ok = true in
    while ok && i < n && Char.is_digit (String.unsafe_get s i) do
      let d = Char.to_int (String.unsafe_get s i) - 48 in
      if v > (Int.max_value - d) / 10 then ok <- false else v <- (v * 10) + d;
      i <- i + 1
    done;
    if ok then v else -1)
;;

let[@zero_alloc] channel s =
  let at = find_key s ~from:0 ~stop:(String.length s) {|"channel"|} in
  let close = if at < 0 then -1 else string_close s at in
  if close < 0
  then Channel.Other
  else (
    let pos = at + 1
    and len = close - at - 1 in
    if equal_sub s ~pos ~len "trades"
    then Trades
    else if equal_sub s ~pos ~len "l2Book"
    then L2_book
    else if equal_sub s ~pos ~len "activeAssetCtx"
    then Active_asset_ctx
    else Other)
;;

let[@zero_alloc] first_element s pos =
  let n = String.length s in
  if pos < 0 || pos >= n || not (Char.equal (String.unsafe_get s pos) '[')
  then -2
  else (
    let i = skip_ws s (pos + 1) in
    if i >= n then -2 else if Char.equal (String.unsafe_get s i) ']' then -1 else i)
;;

let[@zero_alloc] next_element s pos =
  let n = String.length s in
  let i = skip_ws s pos in
  if i >= n
  then -2
  else (
    let c = String.unsafe_get s i in
    if Char.equal c ']'
    then -1
    else if Char.equal c ','
    then (
      let j = skip_ws s (i + 1) in
      if j >= n then -2 else j)
    else -2)
;;

(* One pass over a flat run of key/value pairs. [Pair.read s c at] reads the pair whose
   key opens at [at]: string, scalar, array and object values are all stepped over. *)
module Pair = struct
  type t =
    { mutable key : int
    ; mutable key_len : int
    ; mutable value : int
    ; mutable next : int
    }

  let create () = { key = 0; key_len = 0; value = 0; next = 0 }

  (* Position after the bare scalar (number, true, false, null) at [v], or -1. *)
  let[@zero_alloc] scalar_end s v =
    let n = String.length s in
    let mutable e = v in
    while
      e < n
      &&
      let c = String.unsafe_get s e in
      not
        (Char.equal c ','
         || Char.equal c '}'
         || Char.equal c ']'
         || Char.equal c '{'
         || Char.equal c '['
         || Char.equal c '"'
         || is_ws c)
    do
      e <- e + 1
    done;
    if e = v then -1 else e
  ;;

  (* 1 when another pair follows ([c.next] is its key), 0 at the closing ['}'] ([c.next]
     is just after it), -1 on anything else. *)
  let[@zero_alloc] read s c at =
    let n = String.length s in
    let key_close = if at < n then string_close s at else -1 in
    let colon = if key_close < 0 then n else skip_ws s (key_close + 1) in
    if colon >= n || not (Char.equal (String.unsafe_get s colon) ':')
    then -1
    else (
      let v = skip_ws s (colon + 1) in
      let stop =
        if v >= n
        then -1
        else (
          let c = String.unsafe_get s v in
          if Char.equal c '"'
          then (
            let close = string_close s v in
            if close < 0 then -1 else close + 1)
          else if Char.equal c '[' || Char.equal c '{'
          then value_end s v
          else scalar_end s v)
      in
      let k = if stop < 0 then n else skip_ws s stop in
      if k >= n
      then -1
      else (
        c.key <- at + 1;
        c.key_len <- key_close - at - 1;
        c.value <- v;
        let sep = String.unsafe_get s k in
        if Char.equal sep ','
        then (
          c.next <- skip_ws s (k + 1);
          1)
        else if Char.equal sep '}'
        then (
          c.next <- k + 1;
          0)
        else -1))
  ;;

  let[@zero_alloc] is s c name = equal_sub s ~pos:c.key ~len:c.key_len name
end

module Trade = struct
  type t =
    { mutable coin_pos : int
    ; mutable coin_len : int
    ; mutable buy : bool
    ; mutable px : int
    ; mutable sz : int
    ; mutable time : int
    ; mutable tid : int
    ; pair : Pair.t
    }

  let create () =
    { coin_pos = 0
    ; coin_len = 0
    ; buy = false
    ; px = 0
    ; sz = 0
    ; time = 0
    ; tid = 0
    ; pair = Pair.create ()
    }
  ;;

  (* One pass over the object's pairs; a missing or repeated field refuses it. *)
  let[@zero_alloc] read_at s p t =
    let c = t.pair in
    let mutable coin = -1 in
    let mutable side = -1 in
    let mutable px = -1 in
    let mutable sz = -1 in
    let mutable time = -1 in
    let mutable tid = -1 in
    let mutable state = 1 in
    let mutable at = skip_ws s (p + 1) in
    while state = 1 do
      state <- Pair.read s c at;
      if state >= 0
      then (
        at <- c.next;
        let v = c.value in
        if Pair.is s c "coin"
        then coin <- (if coin = -1 then v else -2)
        else if Pair.is s c "side"
        then side <- (if side = -1 then v else -2)
        else if Pair.is s c "px"
        then px <- (if px = -1 then decimal s v else -2)
        else if Pair.is s c "sz"
        then sz <- (if sz = -1 then decimal s v else -2)
        else if Pair.is s c "time"
        then time <- (if time = -1 then int_at s v else -2)
        else if Pair.is s c "tid"
        then tid <- (if tid = -1 then int_at s v else -2))
    done;
    let coin_close = if coin < 0 then -1 else string_close s coin in
    let side_ok =
      side >= 0
      && side + 2 < String.length s
      && Char.equal (String.unsafe_get s side) '"'
      && Char.equal (String.unsafe_get s (side + 2)) '"'
      && (Char.equal (String.unsafe_get s (side + 1)) 'B'
          || Char.equal (String.unsafe_get s (side + 1)) 'A')
    in
    if state < 0
       || coin_close < 0
       || (not side_ok)
       || px < 0
       || sz < 0
       || time < 0
       || tid < 0
    then -1
    else (
      t.coin_pos <- coin + 1;
      t.coin_len <- coin_close - coin - 1;
      t.buy <- Char.equal (String.unsafe_get s (side + 1)) 'B';
      t.px <- px;
      t.sz <- sz;
      t.time <- time;
      t.tid <- tid;
      at)
  ;;
end

let[@zero_alloc] trades_data s =
  let at = find_key s ~from:0 ~stop:(String.length s) {|"data"|} in
  if at >= 0 && at < String.length s && Char.equal (String.unsafe_get s at) '['
  then at
  else -1
;;

module Book = struct
  type t =
    { mutable coin_pos : int
    ; mutable coin_len : int
    ; mutable time : int
    ; mutable n_bids : int
    ; mutable n_asks : int
    ; mutable side_end : int
    ; bid_px : int array
    ; bid_sz : int array
    ; ask_px : int array
    ; ask_sz : int array
    ; pair : Pair.t
    }

  let capacity = 256

  let create () =
    { coin_pos = 0
    ; coin_len = 0
    ; time = 0
    ; n_bids = 0
    ; n_asks = 0
    ; side_end = 0
    ; bid_px = Array.create ~len:capacity 0
    ; bid_sz = Array.create ~len:capacity 0
    ; ask_px = Array.create ~len:capacity 0
    ; ask_sz = Array.create ~len:capacity 0
    ; pair = Pair.create ()
    }
  ;;

  (* The level object at [p], read in one pass; its [px] and [sz] go to slot [i]. The
     position after its ['}'], or -1. *)
  let[@zero_alloc] level s c p px sz i =
    let mutable level_px = -1 in
    let mutable level_sz = -1 in
    let mutable state = 1 in
    let mutable at = skip_ws s (p + 1) in
    while state = 1 do
      state <- Pair.read s c at;
      if state >= 0
      then (
        at <- c.Pair.next;
        if Pair.is s c "px"
        then level_px <- (if level_px = -1 then decimal s c.value else -2)
        else if Pair.is s c "sz"
        then level_sz <- (if level_sz = -1 then decimal s c.value else -2))
    done;
    if state < 0 || level_px < 0 || level_sz < 0
    then -1
    else (
      Array.unsafe_set px i level_px;
      Array.unsafe_set sz i level_sz;
      at)
  ;;

  (* The levels of the side array opening at [pos] into [px] and [sz]: the count, with
     [t.side_end] set to the position after the closing [']'], or -1. *)
  let[@zero_alloc] side s t ~depth pos px sz =
    let n = String.length s in
    let mutable count = 0 in
    let mutable p = first_element s pos in
    let mutable last = pos in
    while p >= 0 && count < depth do
      let stop =
        if Char.equal (String.unsafe_get s p) '{' && count < capacity
        then level s t.pair p px sz count
        else -1
      in
      if stop < 0
      then p <- -2
      else (
        count <- count + 1;
        last <- stop;
        p <- next_element s stop)
    done;
    if p >= 0 && depth >= capacity
    then (* More levels than [capacity]: a full read cannot hold them. *)
      -1
    else if p >= 0
    then (
      (* [depth] levels read; step over the rest without reading them. *)
      let close = value_end s pos in
      if close < 0
      then -1
      else (
        t.side_end <- close;
        count))
    else if p = -1
    then (
      (* [next_element] stopped at the [']']; an empty side stopped right after ['[']. *)
      let close = skip_ws s (if count = 0 then pos + 1 else last) in
      if close < n && Char.equal (String.unsafe_get s close) ']'
      then (
        t.side_end <- close + 1;
        count)
      else -1)
    else -1
  ;;

  let[@zero_alloc] read ~depth s t =
    let n = String.length s in
    let data = find_key s ~from:0 ~stop:n {|"data"|} in
    if data < 0 || data >= n || not (Char.equal (String.unsafe_get s data) '{')
    then false
    else (
      (* The first ["coin"], ["time"] and ["levels"] keys after ["data"] are its own: the
         level objects have none of them. *)
      let coin = find_key s ~from:data ~stop:n {|"coin"|} in
      let coin_close = if coin < 0 then -1 else string_close s coin in
      let time = int_at s (find_key s ~from:data ~stop:n {|"time"|}) in
      let levels = find_key s ~from:data ~stop:n {|"levels"|} in
      let bids = if levels < 0 then -2 else first_element s levels in
      let n_bids =
        if bids >= 0 && Char.equal (String.unsafe_get s bids) '['
        then side s t ~depth bids t.bid_px t.bid_sz
        else -1
      in
      let asks = if n_bids < 0 then -2 else next_element s t.side_end in
      let n_asks =
        if asks >= 0 && Char.equal (String.unsafe_get s asks) '['
        then side s t ~depth asks t.ask_px t.ask_sz
        else -1
      in
      if coin_close < 0
         || time < 0
         || n_asks < 0
         (* [levels] is exactly [bids; asks]. *)
         || next_element s t.side_end <> -1
      then false
      else (
        t.coin_pos <- coin + 1;
        t.coin_len <- coin_close - coin - 1;
        t.time <- time;
        t.n_bids <- n_bids;
        t.n_asks <- n_asks;
        true))
  ;;
end
