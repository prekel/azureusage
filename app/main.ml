open Core
open Js_of_ocaml
open Bonsai_web
open Bonsai.Let_syntax

let field ~default ~pholder =
  let%sub pass, set_pass = Bonsai.state [%here] (module String) ~default_model:default in
  let%arr pass = pass
  and set_pass = set_pass in
  ( pass
  , Vdom.(
      Node.input
        ~attr:
          Attr.(many [ on_input (fun _ -> set_pass); value pass; placeholder pholder ])
        []) )
;;

let login ~submit =
  let%sub host, host_form = field ~default:Mdx.Env.Default.host ~pholder:"host" in
  let%sub port, port_form =
    field ~default:(Int.to_string Mdx.Env.Default.port) ~pholder:"port"
  in
  let%sub ns, ns_form = field ~default:Mdx.Env.Default.ns ~pholder:"ns" in
  let%sub user, user_form = field ~default:Mdx.Env.Default.user ~pholder:"user" in
  let%sub pass, pass_form = field ~default:Mdx.Env.Default.pass ~pholder:"pass" in
  let%arr host = host
  and port = port
  and ns = ns
  and user = user
  and pass = pass
  and host_form = host_form
  and port_form = port_form
  and ns_form = ns_form
  and user_form = user_form
  and pass_form = pass_form
  and submit = submit in
  Vdom.(
    Node.div
      [ host_form
      ; port_form
      ; ns_form
      ; user_form
      ; pass_form
      ; Node.button
          ~attr:
            Attr.(
              many
                [ on_click (fun _ ->
                      let env =
                        Mdx.Env.make ~host ~port:(Int.of_string port) ~ns ~user ~pass
                      in
                      let%bind.Effect status =
                        Effect_lwt.of_unit_lwt (fun () ->
                            let module Mdx = Mdx.Make ((val env)) in
                            let%bind.Lwt r, _ = Mdx.test () in
                            let status = Cohttp.Response.status r in
                            Lwt.return status)
                      in
                      match status with
                      | `OK -> submit env
                      | _ ->
                        Dom_html.window##alert (Js.string "Wrong password");
                        Effect.Ignore)
                ])
          [ Node.text "Login" ]
      ])
;;

let view ~env =
  let%sub res, set_res = Bonsai.state_opt [%here] (module String) in
  let%arr env = env
  and res = res
  and set_res = set_res in
  let module Mdx = Mdx.Make ((val env : Mdx.Env.S)) in
  Vdom.(
    Node.div
      [ begin
          match res with
          | None -> Node.none
          | Some res -> Node.pre [ Node.text res ]
        end
      ; Node.button
          ~attr:
            Attr.(
              many
                [ on_click (fun _ ->
                      let%bind.Effect r =
                        Effect_lwt.of_unit_lwt (fun () ->
                            let query =
                              {|SELECT NON EMPTY NONEMPTYCROSSJOIN([Date].[H1].[Month].Members,[Measures].[Cost]) ON 0,NON EMPTY [ServiceRegion].[H1].[ServiceRegion].Members ON 1 FROM [AZUREUSAGE]|}
                            in
                            let%bind.Lwt _r, b = Mdx.mdx query in
                            let%bind.Lwt b = Cohttp_lwt.Body.to_string b in
                            Firebug.console##log (b |> Js.string);
                            Lwt.return b)
                      in
                      set_res (Some r))
                ])
          [ Node.text "query" ]
      ])
;;

let app =
  let%sub env, set_env =
    Bonsai.state_opt
      [%here]
      (module struct
        type t = (module Mdx.Env.S)

        let sexp_of_t _ = Sexp.List []
        let t_of_sexp _ = (module Mdx.Env.Default : Mdx.Env.S)
        let equal = Poly.equal
      end)
  in
  let%sub submit =
    let%arr set_env = set_env in
    fun s -> set_env (Some s)
  in
  match%sub env with
  | Some env -> view ~env
  | None -> login ~submit
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app
;;
