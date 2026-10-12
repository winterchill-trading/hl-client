(** JSON-over-HTTP POST used by [Info] and [Exchange], on top of [Venue_net.Http]. *)

open! Core
open Async

module Response : sig
  type t =
    { status : int
    ; body : string
    }
  [@@deriving sexp_of]
end

(** [post_json uri json] POSTs [json] with [Content-Type: application/json] plus any extra
    [headers] and reads the whole response body. Connection and transport failures become
    [Error]; any HTTP status is returned as [Ok]. *)
val post_json
  :  ?headers:(string * string) list
  -> Uri.t
  -> Yojson.Safe.t
  -> Response.t Or_error.t Deferred.t

(** [decode_json response ~f] parses the body as JSON and applies [f]; parse and decode
    exceptions, and a non-2xx status, become an [Error] that includes the status and the
    body. *)
val decode_json : Response.t -> f:(Yojson.Safe.t -> 'a) -> 'a Or_error.t
