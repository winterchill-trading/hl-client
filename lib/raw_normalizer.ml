open! Core

type t = Normalizer.t

let venue = Market_data.Venue.Hyperliquid
let create () = Normalizer.create ()
let frame = Normalizer.frame
let disconnect = Normalizer.reset
