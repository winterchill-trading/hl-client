open! Core
open Async

module Request = struct
  type t =
    { path : string
    ; x_address : string option
    ; body : string
    }
end

type t =
  { server : (Socket.Address.Inet.t, int) Cohttp_async.Server.t
  ; requests : Request.t Queue.t
  }

let start ?(response = {|{"status":"ok","response":{"type":"default"}}|}) () =
  let requests = Queue.create () in
  let%map server =
    Cohttp_async.Server.create
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.bind_to Localhost On_port_chosen_by_os)
      (fun ~body _ request ->
         let%bind body = Cohttp_async.Body.to_string body in
         Queue.enqueue
           requests
           { Request.path = Uri.path (Cohttp.Request.uri request)
           ; x_address = Cohttp.Header.get (Cohttp.Request.headers request) "x-address"
           ; body
           };
         Cohttp_async.Server.respond_string response)
  in
  { server; requests }
;;

let base_url t =
  Uri.of_string
    (sprintf "http://127.0.0.1:%d" (Cohttp_async.Server.listening_on t.server))
;;

let requests t = Queue.to_list t.requests
let close t = Cohttp_async.Server.close t.server
