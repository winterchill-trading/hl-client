(** {!Normalizer} as a {!Market_data.Normalizer.S}, so [rebuild-norm] can turn
    market-recorder raw files of Hyperliquid back into events:
    [Market_data.Rebuild_norm.command [ (module Hl_client.Raw_normalizer) ]].

    Every stream is produced: an [l2Book] frame gives an [L2] snapshot and, when the top
    changed, a [Bbo], as {!Live_feed} does with both requested. The recorder writes each
    [l2Book] frame once (under [l2], or [bbo] when only [Bbo] was recorded), so the two
    streams of a rebuilt day come from one raw series. A [disconnect] forgets the last top
    of book, as a reconnect of {!Live_feed} does. *)

include Market_data.Normalizer.S with type t = Normalizer.t
