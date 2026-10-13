open! Core
open Async

module Response = struct
  type t =
    { status : int
    ; body : string
    }
  [@@deriving sexp_of]
end

let post_json ?headers uri json =
  Venue_net.Http.post_json ?headers uri json
  |> Deferred.Or_error.map ~f:(fun (status, body) -> { Response.status; body })
;;

let get ?headers uri =
  Venue_net.Http.get ?headers uri
  |> Deferred.Or_error.map ~f:(fun (status, body) -> { Response.status; body })
;;

let decode_json ({ status; body } : Response.t) ~f =
  Venue_net.Http.decode_json (status, body) ~f
;;
