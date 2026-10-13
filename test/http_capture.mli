(** A one-route HTTP server on 127.0.0.1 for tests: answers every POST with [response] and
    records the path, the [X-Address] header and the body of each request. *)

open! Core
open Async

module Request : sig
  type t =
    { path : string
    ; x_address : string option
    ; body : string
    }
end

type t

(** [respond], when given, computes the response body from the request instead. *)
val start : ?response:string -> ?respond:(Request.t -> string) -> unit -> t Deferred.t

(** [http://127.0.0.1:<port>]. *)
val base_url : t -> Uri.t

(** Requests so far, oldest first. *)
val requests : t -> Request.t list

val close : t -> unit Deferred.t
