open Lwt
module S = Cohttp_lwt_unix.Server

let make_server f port =
  let callback conn_id req body =
    lwt body = Cohttp_lwt_body.to_string body in
    S.respond_string ~status:`OK ~body:(f body) ()
  in
  let conn_closed conn_id () = () in
  S.create ~address:"0.0.0.0" ~port {S.callback; conn_closed}

let run f p = Lwt_main.run (make_server f p)
