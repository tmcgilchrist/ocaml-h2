open Core
open Async
open H2
open H2_async

let error_handler _ ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  (match error with
  | `Exn exn ->
    Body.Writer.write_string response_body (Exn.to_string exn);
    Body.Writer.write_string response_body "\n"
  | #Status.standard as error ->
    Body.Writer.write_string response_body (Status.default_reason_phrase error));
  Body.Writer.close response_body

let request_handler sock reqd =
  eprintf "Received request from %s\n%!" (Socket.Address.Inet.to_string sock);
  match Reqd.request reqd with
  | { Request.meth = `POST; headers; _ } ->
    let response =
      let content_type =
        match Headers.get headers "content-type" with
        | None -> "application/octet-stream"
        | Some x -> x
      in
      Response.create
        ~headers:(Headers.of_list [ "content-type", content_type ])
        `OK
    in
    let request_body = Reqd.request_body reqd in
    let response_body = Reqd.respond_with_streaming reqd response in
    let rec on_read buffer ~off ~len =
      Body.Writer.write_bigstring response_body buffer ~off ~len;
      Body.Reader.schedule_read request_body ~on_eof ~on_read
    and on_eof () =
      print_endline "eof";
      Body.Writer.close response_body
    in
    Body.Reader.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
  | _ -> Reqd.respond_with_string reqd (Response.create `Method_not_allowed) ""

let main port () =
  let where_to_listen =
    Tcp.Where_to_listen.bind_to
      Tcp.Bind_to_address.Localhost
      (Tcp.Bind_to_port.On_port port)
  in
  let socket = Unix.Socket.create Unix.Socket.Type.tcp in
  (* let a = Tcp.(Server.create_sock *)
  (*         ~on_handler_error:`Ignore *)
  (*         ~backlog:10_000 *)
  (*         ~max_connections:10_000 *)
  (*         ~max_accepts_per_batch *)
  (*         where_to_listen) in *)
  (* $ dune exec -- examples/async/async_tls_https_echo_server_post.exe *)
  (*                                 (monitor.ml.Error                      *)
  (*                                    ("Writer error from inner_monitor" *)
  (*                                       (Unix.Unix_error "Socket is not connected" writev_assume_fd_is_nonblocking *)
  (*                                          "") *)
  (*                                       (writer *)
  (*                                          ((file_descr 5) (info ((type_ tcp) (listening_on 127.0.0.1:8080))) *)
  (*                                             (kind (Socket Passive))))) *)
  (*                                    ("Caught by monitor (id 17)")) *)

  let config = Config.default in
  (Server.TLS.create_connection_handler_with_default
     ~config
     ~certfile:"./certificates/server.pem"
     ~keyfile:"./certificates/server.key"
     ~request_handler
     ~error_handler where_to_listen socket)
  >>= fun _server -> Deferred.never ()

let () =
  Command.async_spec
    ~summary:"Start a hello world async_ssl server"
    Command.Spec.(
      empty
      +> flag
           "-port"
           (optional_with_default 8080 int)
           ~doc:"int Source port to listen on")
    main
  |> Command_unix.run
