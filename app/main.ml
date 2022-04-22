open Core
open Js_of_ocaml
open Bonsai_web
open Bonsai.Let_syntax

let field ?is_password ~pholder default =
  let%sub state, set_state =
    Bonsai.state [%here] (module String) ~default_model:default
  in
  let%arr state = state
  and set_state = set_state in
  ( state
  , Vdom.(
      Node.input
        ~attr:
          Attr.(
            many
              [ on_input (fun _ -> set_state)
              ; value state
              ; placeholder pholder
              ; type_
                  (match is_password with
                  | Some true -> "password"
                  | _ -> "text")
              ])
        []) )
;;

let select ~(sexp_of_option : 'a -> _) ~(option_of_sexp : _ -> 'a) ~pholder options =
  let%sub state, set_state = Bonsai.state_opt [%here] (module Sexp) in
  let%arr state = state
  and set_state = set_state
  and options = options in
  let opts =
    List.map options ~f:(fun option ->
        Vdom.(
          Node.option
            ~attr:Attr.(many [ value (option |> sexp_of_option |> Sexp.to_string_hum) ])
            [ Node.text
                (match option |> sexp_of_option with
                | Sexp.Atom str -> str
                | sexp -> Sexp.to_string_hum sexp)
            ]))
  in
  ( (match state with
    | Some s -> Some (option_of_sexp s)
    | _ -> None)
  , Vdom.(
      Node.select
        ~attr:
          Attr.(
            many
              [ on_change (fun _ s ->
                    let state =
                      match Sexp.of_string s with
                      | Sexp.List [] -> None
                      | s -> Some s
                    in
                    set_state state)
              ])
        (Node.option
           ~attr:Attr.(many [ value (Sexp.List [] |> Sexp.to_string_hum) ])
           [ Node.text (" Select " ^ pholder) ]
        :: opts)) )
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

let frm ~chart_widget mdx =
  let%sub columns, set_columns =
    Bonsai.state
      [%here]
      (module struct
        type t = string list [@@deriving sexp, equal]
      end)
      ~default_model:[]
  in
  let%sub dimension, select_dimension =
    Bonsai.Value.return Queries.all_data_dimensions
    |> select
         ~sexp_of_option:[%sexp_of: Queries.data_dimension]
         ~option_of_sexp:[%of_sexp: Queries.data_dimension]
         ~pholder:"Dimension"
  in
  let%sub measure, select_measure =
    Bonsai.Value.return Queries.all_measures
    |> select
         ~sexp_of_option:[%sexp_of: Queries.measure]
         ~option_of_sexp:[%of_sexp: Queries.measure]
         ~pholder:"Measure"
  in
  let%sub data, set_data =
    Bonsai.state_opt
      [%here]
      (module struct
        type t = (string * ((int * string) * float option) list) list
        [@@deriving sexp, equal]
      end)
  in
  let fq ~set_data ~mdx:(module Mdx : Mdx.S) ~column ~dimension ~measure =
    match column, dimension, measure with
    | Some column, Some dimension, Some measure ->
      (match%bind.Effect
         Effect_lwt.of_unit_lwt (fun () ->
             Queries.query_by_time ~data:dimension ~time:`Day ~measure [ column ]
             |> Mdx.mdx)
       with
      | Ok r -> Some (Queries.by_time r) |> set_data
      | Error _ -> assert false)
    | _ -> Effect.Ignore
  in
  let%sub on_column_change =
    let%arr set_data = set_data
    and dimension = dimension
    and measure = measure
    and mdx = mdx in
    fun column -> fq ~set_data ~mdx ~column ~dimension ~measure
  in
  let%sub column, select_column =
    columns
    |> select
         ~sexp_of_option:[%sexp_of: string]
         ~option_of_sexp:[%of_sexp: string]
         ~pholder:"column"
  in
  let%sub on_data_dimension_change =
    let%arr set_columns = set_columns
    and set_data = set_data
    and column = column
    and measure = measure
    and (module Mdx : Mdx.S) = mdx in
    fun dimension ->
      let%bind.Effect () =
        match dimension with
        | Some dim ->
          (match%bind.Effect
             Effect_lwt.of_unit_lwt (fun () ->
                 Queries.query_columns_by_dimension ~data:dim |> Mdx.mdx)
           with
          | Ok r -> Queries.columns_by_dimension r |> set_columns
          | Error _ -> assert false)
        | None -> set_columns []
      in
      fq ~set_data ~mdx:(module Mdx) ~column ~dimension ~measure
  in
  let%sub on_measure_change =
    let%arr set_data = set_data
    and column = column
    and dimension = dimension
    and (module Mdx : Mdx.S) = mdx in
    fun measure -> fq ~set_data ~mdx:(module Mdx) ~column ~dimension ~measure
  in
  let%sub () =
    Bonsai.Edge.on_change
      [%here]
      (module struct
        type t = Queries.data_dimension option [@@deriving sexp, equal]
      end)
      dimension
      ~callback:on_data_dimension_change
  in
  let%sub () =
    Bonsai.Edge.on_change
      [%here]
      (module struct
        type t = Queries.measure option [@@deriving sexp, equal]
      end)
      measure
      ~callback:on_measure_change
  in
  let%sub () =
    Bonsai.Edge.on_change
      [%here]
      (module struct
        type t = string option [@@deriving sexp, equal]
      end)
      column
      ~callback:on_column_change
  in
  let%arr select_dimension = select_dimension
  and select_measure = select_measure
  and select_column = select_column
  and data = data in
  Vdom.(
    Node.div
      [ select_dimension
      ; select_measure
      ; select_column
      ; (match data with
        | Some [ (_col, d) ] ->
          let labels, data = List.unzip d in
          (chart_widget |> Staged.unstage)
            Chart.Widget.Input.
              { labels = labels |> List.map ~f:snd |> List.to_array
              ; data = data |> List.map ~f:(Option.value ~default:0.) |> List.to_array
              }
        | None -> Node.none
        | _ -> Node.none)
      ])
;;

let view ~chart_widget ~env =
  let%sub mdx =
    let%arr env = env in
    (module Mdx.Make ((val env : Mdx.Env.S)) : Mdx.S)
  in
  frm ~chart_widget mdx
;;

let app ~chart_widget =
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
  | Some env -> view ~chart_widget ~env
  | None -> login ~submit
;;

let (_ : _ Start.Handle.t) =
  let chart_widget = Vdom.Node.widget_of_module (module Chart.Widget) in
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    (app ~chart_widget)
;;
