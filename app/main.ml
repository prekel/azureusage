open Core
open Js_of_ocaml
open Bonsai_web
open Bonsai.Let_syntax

let field ?is_password ~pholder default =
  let%sub pass, set_pass = Bonsai.state [%here] (module String) ~default_model:default in
  let%arr pass = pass
  and set_pass = set_pass in
  ( pass
  , Vdom.(
      Node.input
        ~attr:
          Attr.(
            many
              [ on_input (fun _ -> set_pass)
              ; value pass
              ; placeholder pholder
              ; type_
                  (match is_password with
                  | Some true -> "password"
                  | _ -> "text")
              ])
        []) )
;;

let login ~submit =
  let%sub host, host_form = field Mdx.Env.Default.host ~pholder:"host" in
  let%sub port, port_form = field (Int.to_string Mdx.Env.Default.port) ~pholder:"port" in
  let%sub ns, ns_form = field Mdx.Env.Default.ns ~pholder:"ns" in
  let%sub user, user_form = field Mdx.Env.Default.user ~pholder:"user" in
  let%sub pass, pass_form =
    field Mdx.Env.Default.pass ~pholder:"pass" ~is_password:true
  in
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
                      let module Mdx = Mdx.Make ((val env)) in
                      let%bind.Effect r =
                        Effect_lwt.of_unit_lwt (fun () ->
                            let%bind.Lwt r = Mdx.test () in
                            Lwt.return r)
                      in
                      match r with
                      | Ok () -> submit env
                      | Error err ->
                        Dom_html.window##alert
                          ([%sexp (err : Mdx.http_error)]
                          |> Sexp.to_string_hum
                          |> Js.string);
                        Effect.Ignore)
                ])
          [ Node.text "Login" ]
      ])
;;

let view ~env =
  let%sub res, set_res =
    Bonsai.state_opt
      [%here]
      (module struct
        type t = float option list [@@deriving sexp, equal]
      end)
  in
  let%arr env = env
  and res = res
  and set_res = set_res in
  let module Mdx = Mdx.Make ((val env : Mdx.Env.S)) in
  Vdom.(
    Node.div
      [ begin
          match res with
          | None -> Node.none
          | Some res ->
            Node.pre [ Node.text (Sexp.to_string_hum [%sexp (res : float option list)]) ]
        end
      ; Node.button
          ~attr:
            Attr.(
              many
                [ on_click (fun _ ->
                      let%bind.Effect nums =
                        Effect_lwt.of_unit_lwt (fun () ->
                            let query =
                              {|SELECT NON EMPTY NONEMPTYCROSSJOIN([Date].[H1].[Month].Members,[Measures].[Cost]) ON 0,NON EMPTY [ServiceRegion].[H1].[ServiceRegion].Members ON 1 FROM [AZUREUSAGE]|}
                            in
                            let%bind.Lwt nums = Mdx.mdx query in
                            Lwt.return nums)
                      in
                      match nums with
                      | Ok nums -> set_res (Some nums)
                      | Error (#Mdx.http_error as status) ->
                        Dom_html.window##alert
                          ([%sexp (status : Mdx.http_error)]
                          |> Sexp.to_string_hum
                          |> Js.string);
                        Effect.Ignore
                      | Error (`WrondData j) ->
                        printf "WrondData\n";
                        Log.yojson j;
                        Effect.Ignore)
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

        let sexp_of_t = Mdx.Env.to_sexp
        let t_of_sexp = Mdx.Env.of_sexp
        let equal = Mdx.Env.equal
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
