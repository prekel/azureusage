module Env = struct
  module type S = sig
    val host : string
    val port : int
    val ns : string
    val user : string
    val pass : string
  end

  module Default : S = struct
    let host = "10.49.0.1"
    let port = 9092
    let ns = "AZUREUSAGE"
    let user = "_SYSTEM"
    let pass = ""
  end

  let make ~host ~port ~ns ~user ~pass =
    (module struct
      let host = host
      let port = port
      let ns = ns
      let user = user
      let pass = pass
    end : S)
  ;;
end

module type S = sig
  val mdx : string -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
end

module Make (Env : Env.S) : S = struct
  let make_url = Uri.make ~host:Env.host ~port:Env.port

  let auth_headers =
    Cohttp.Header.add_authorization (Cohttp.Header.init ()) (`Basic (Env.user, Env.pass))
  ;;

  let _test () =
    let url = make_url ~path:"MDX2JSON/Test" ~query:[ "Namespace", [ Env.ns ] ] () in
    Cohttp_lwt_jsoo.Client.get ~headers:auth_headers url
  ;;

  let mdx query =
    let url = make_url ~path:"MDX2JSON/MDX" ~query:[ "Namespace", [ Env.ns ] ] () in
    Cohttp_lwt_jsoo.Client.post
      ~body:
        (`Assoc [ "MDX", `String query ]
        |> Yojson.Safe.to_string
        |> Cohttp_lwt.Body.of_string)
      ~headers:auth_headers
      url
  ;;
end
