
let () =
  let open Lwt.Infix in
  let port = ref 8080 in
  let host = ref "localhost" in
  Arg.parse
    [ "-port", Arg.Set_int port, " Listening port number (8080 by default)";
      "-host", Arg.Set_string host, "HOST to connect to"
    ]
    ignore
    "Start a hello world tls-async client";
  Lwt_main.run (main host port)