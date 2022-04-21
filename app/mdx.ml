module Make (Env : sig
  val host : string
  val port : int
  val ns : string
  val user : string
  val pass : string
end) =
struct
  let make_url = Uri.make ~host:Env.host ~port:Env.port

  let auth_headers =
    Cohttp.Header.add_authorization (Cohttp.Header.init ()) (`Basic (Env.user, Env.pass))
  ;;

  let test () =
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
