open! Core
open Async

module Response = struct
  type t =
    { status : int
    ; body : string
    }
  [@@deriving sexp_of]
end

let post_json ?(headers = []) uri json =
  let headers = Cohttp.Header.of_list (("Content-Type", "application/json") :: headers) in
  Monitor.try_with_or_error ~extract_exn:true (fun () ->
    let%bind response, body =
      Cohttp_async.Client.post
        ~headers
        ~body:(Cohttp_async.Body.of_string (Yojson.Safe.to_string json))
        uri
    in
    let%map body = Cohttp_async.Body.to_string body in
    let status = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
    { Response.status; body })
  |> Deferred.Or_error.tag_s ~tag:[%message "POST" ~uri:(Uri.to_string uri : string)]
;;

let decode_json (response : Response.t) ~f =
  let result =
    if response.status >= 200 && response.status < 300
    then Or_error.try_with (fun () -> f (Yojson.Safe.from_string response.body))
    else Or_error.error_string "unexpected HTTP status"
  in
  Or_error.tag_s
    result
    ~tag:[%message "" ~status:(response.status : int) ~body:(response.body : string)]
;;
