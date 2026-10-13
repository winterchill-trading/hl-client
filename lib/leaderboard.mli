(** The trader leaderboard from Hyperliquid's stats host ({!Hl_protocol.Leaderboard}).

    The host is [stats-data.hyperliquid.xyz], separate from the API; the response is the
    whole board (about 20 MB, 25k rows on testnet in October 2026), so fetch it rarely and
    cache it. *)

open! Core
open Async
open Hl_protocol

(** [fetch ?network ()] GETs [Leaderboard.url network] (default [`Mainnet]); [uri]
    overrides the URL, e.g. for tests. Read-only. *)
val fetch
  :  ?network:[ `Mainnet | `Testnet ]
  -> ?uri:Uri.t
  -> unit
  -> Leaderboard.t Or_error.t Deferred.t

(** The best [limit] rows by [by] in [window], skipping rows without that window or with
    an account value below [min_account_value] (default 0). Ties keep the input order. *)
val rank
  :  ?min_account_value:float
  -> Leaderboard.t
  -> window:Leaderboard.Window.t
  -> by:[ `Pnl | `Roi | `Volume ]
  -> limit:int
  -> (Leaderboard.Row.t * Leaderboard.Performance.t) list
