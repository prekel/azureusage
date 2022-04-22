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
              ; placeholder pholder
              ])
        (Node.option
           ~attr:Attr.(many [ value (Sexp.List [] |> Sexp.to_string_hum) ])
           [ Node.text "" ]
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

let _view ~env =
  let%sub res, set_res =
    Bonsai.state_opt
      [%here]
      (module struct
        type t = string list [@@deriving sexp, equal]
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
            Node.pre [ Node.text (Sexp.to_string_hum [%sexp (res : string list)]) ]
        end
      ; Node.button
          ~attr:
            Attr.(
              many
                [ on_click (fun _ ->
                      let%bind.Effect nums =
                        Effect_lwt.of_unit_lwt (fun () ->
                            let query =
                              Queries.query_columns_by_dimension ~data:`ServiceType
                            in
                            let%bind.Lwt nums = Mdx.mdx query in
                            Lwt.return nums)
                      in
                      match nums with
                      | Ok nums -> set_res (Some (Queries.columns_by_dimension nums))
                      | Error (#Mdx.http_error as status) ->
                        Dom_html.window##alert
                          ([%sexp (status : Mdx.http_error)]
                          |> Sexp.to_string_hum
                          |> Js.string);
                        Effect.Ignore
                      | Error (`WrondData (_, pos)) ->
                        print_s [%message "WrondData" ~pos:(pos : Utils.LexingPosition.t)];
                        Effect.Ignore)
                ])
          [ Node.text "query" ]
      ])
;;

module Make_OjsT_from_JsT (M : sig
  type t
end) : Ojs.T with type t = M.t = struct
  type t = M.t

  let t_of_js = Obj.magic
  let t_to_js = Obj.magic
end

module ChartJS = struct
  module Context = Make_OjsT_from_JsT (struct
    type t = Dom_html.canvasRenderingContext2D Js.t
  end)

  module Chart =
  [%js:
  type t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t

  type dataset =
    { label : string
    ; data : float array
    ; backgroundColor : string array
    ; borderColor : string array
    ; borderWidth : int
    }

  type data =
    { labels : string array
    ; datasets : dataset array
    }

  type params =
    { type_ : string [@js "type"]
    ; data : data
    }

  val create : Context.t -> params -> t [@@js.new "Chart"]
  val destroy : t -> unit -> unit [@@js.call]]
end

module ChartJSWidget = struct
  type dom = Dom_html.canvasElement

  module Input = struct
    type t =
      { labels : string array
      ; data : float array
      }
    [@@deriving sexp_of]
  end

  module State = struct
    type t =
      { context : (Dom_html.canvasRenderingContext2D Js.t[@sexp.opaque])
      ; chart : (ChartJS.Chart.t[@sexp.opaque])
      }
    [@@deriving sexp_of]
  end

  let name = "ChartJSWidget"

  let create i =
    let canvas = Dom_html.createCanvas Dom_html.document in
    let context = canvas##getContext Dom_html._2d_ in
    let chart =
      ChartJS.Chart.create
        context
        { type_ = "bar"
        ; data =
            { labels = i.Input.labels
            ; datasets =
                [| { label = ""
                   ; data = i.data
                   ; backgroundColor =
                       [| "rgba(255, 99, 132, 0.2)"
                        ; "rgba(54, 162, 235, 0.2)"
                        ; "rgba(255, 206, 86, 0.2)"
                        ; "rgba(75, 192, 192, 0.2)"
                        ; "rgba(153, 102, 255, 0.2)"
                        ; "rgba(255, 159, 64, 0.2)"
                       |]
                   ; borderColor =
                       [| "rgba(255, 99, 132, 1)"
                        ; "rgba(54, 162, 235, 1)"
                        ; "rgba(255, 206, 86, 1)"
                        ; "rgba(75, 192, 192, 1)"
                        ; "rgba(153, 102, 255, 1)"
                        ; "rgba(255, 159, 64, 1)"
                       |]
                   ; borderWidth = 1
                   }
                |]
            }
        }
    in
    State.{ context; chart }, canvas
  ;;

  let update
      ~prev_input:_
      ~input:Input.{ data; labels }
      ~state:State.{ context; chart }
      ~element:canvas
    =
    ChartJS.Chart.destroy chart ();
    let chart =
      ChartJS.Chart.create
        context
        { type_ = "bar"
        ; data =
            { labels
            ; datasets =
                [| { label = ""
                   ; data
                   ; backgroundColor =
                       [| "rgba(255, 99, 132, 0.2)"
                        ; "rgba(54, 162, 235, 0.2)"
                        ; "rgba(255, 206, 86, 0.2)"
                        ; "rgba(75, 192, 192, 0.2)"
                        ; "rgba(153, 102, 255, 0.2)"
                        ; "rgba(255, 159, 64, 0.2)"
                       |]
                   ; borderColor =
                       [| "rgba(255, 99, 132, 1)"
                        ; "rgba(54, 162, 235, 1)"
                        ; "rgba(255, 206, 86, 1)"
                        ; "rgba(75, 192, 192, 1)"
                        ; "rgba(153, 102, 255, 1)"
                        ; "rgba(255, 159, 64, 1)"
                       |]
                   ; borderWidth = 1
                   }
                |]
            }
        }
    in
    State.{ context; chart }, canvas
  ;;

  let destroy ~prev_input ~state ~element =
    let _ = prev_input, state, element in
    ()
  ;;
end

let chartjs =
  let w = Vdom.Node.widget_of_module (module ChartJSWidget) in
  let s = w |> Staged.unstage in
  s
;;

let frm ~(chart_widget : (ChartJSWidget.Input.t -> Vdom.Node.t) Staged.t) mdx =
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
         ~pholder:"Selected"
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
  let%arr _columns = columns
  and _dimension = dimension
  and select_dimension = select_dimension
  and _measure = measure
  and select_measure = select_measure
  and _column = column
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
            ChartJSWidget.Input.
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
  let chart_widget = Vdom.Node.widget_of_module (module ChartJSWidget) in
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    (app ~chart_widget)
;;
