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

let select
    ~(sexp_of_option : 'a -> _)
    ~(option_of_sexp : _ -> 'a)
    ~pholder
    ?onchange
    options
  =
  let%sub state, set_state = Bonsai.state_opt [%here] (module Sexp) in
  let onchange =
    match onchange with
    | Some onchange -> Bonsai.Value.map onchange ~f:(fun a -> Some a)
    | None -> Bonsai.Value.return None
  in
  let%arr state = state
  and set_state = set_state
  and onchange = onchange
  and options = options in
  let opts =
    List.map options ~f:(fun option ->
        Vdom.(
          Node.option
            ~attr:Attr.(many [ value (option |> sexp_of_option |> Sexp.to_string_hum) ])
            [ Node.text (option |> sexp_of_option |> Sexp.to_string_hum) ]))
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
                    let%bind.Effect () =
                      match onchange with
                      | Some onchange ->
                        onchange
                          (match state with
                          | Some s -> Some (option_of_sexp s)
                          | _ -> None)
                      | None -> Effect.Ignore
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
    ; data : int array
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

  val create : Context.t -> params -> t [@@js.new "Chart"]]
end

module ChartJSWidget = struct
  type dom = Dom_html.canvasElement

  module Input = struct
    type t = unit [@@deriving sexp_of]
  end

  module State = struct
    type t = unit [@@deriving sexp_of]
  end

  let name = "ChartJSWidget"

  let create i =
    let _ = i in
    let el = Dom_html.createCanvas Dom_html.document in
    let ctx = el##getContext Dom_html._2d_ in
    let _c =
      ChartJS.Chart.create
        ctx
        { type_ = "bar"
        ; data =
            { labels = [| "qwf" |]
            ; datasets =
                [| { label = "# of Votes"
                   ; data = [| 12; 19; 3; 5; 2; 3 |]
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
    (), el
  ;;

  let update ~prev_input ~input ~state ~element =
    let _ = prev_input, input in
    state, element
  ;;

  let destroy ~prev_input ~state ~element =
    let _ = prev_input, state, element in
    ()
  ;;
end

let chartjs =
  let w = Vdom.Node.widget_of_module (module ChartJSWidget) in
  let s = w |> Staged.unstage in
  s ()
;;

let frm mdx =
  let%sub columns, set_columns =
    Bonsai.state
      [%here]
      (module struct
        type t = string list [@@deriving sexp, equal]
      end)
      ~default_model:[]
  in
  let%sub onchange =
    let%arr set_columns = set_columns
    and (module Mdx : Mdx.S) = mdx in
    function
    | Some dim ->
      (match%bind.Effect
         Effect_lwt.of_unit_lwt (fun () ->
             Queries.query_columns_by_dimension ~data:dim |> Mdx.mdx)
       with
      | Ok r -> Queries.columns_by_dimension r |> set_columns
      | Error _ -> assert false)
    | None -> set_columns []
  in
  let%sub dimension, select_dimension =
    Bonsai.Value.return Queries.all_data_dimensions
    |> select
         ~sexp_of_option:[%sexp_of: Queries.data_dimension]
         ~option_of_sexp:[%of_sexp: Queries.data_dimension]
         ~pholder:"Dimension"
         ~onchange
  in
  let%sub measure, select_measure =
    Bonsai.Value.return Queries.all_measures
    |> select
         ~sexp_of_option:[%sexp_of: Queries.measure]
         ~option_of_sexp:[%of_sexp: Queries.measure]
         ~pholder:"Measure"
  in
  let%sub column, select_column =
    columns
    |> select
         ~sexp_of_option:[%sexp_of: string]
         ~option_of_sexp:[%of_sexp: string]
         ~pholder:"Selected"
  in
  let%arr columns = columns
  and dimension = dimension
  and select_dimension = select_dimension
  and measure = measure
  and select_measure = select_measure
  and column = column
  and select_column = select_column in
  Vdom.(
    Node.div
      [ Node.pre [ Node.text (Sexp.to_string_hum [%sexp (columns : string list)]) ]
      ; Node.pre
          [ Node.text
              (Sexp.to_string_hum [%sexp (dimension : Queries.data_dimension option)])
          ]
      ; select_dimension
      ; Node.pre
          [ Node.text (Sexp.to_string_hum [%sexp (measure : Queries.measure option)]) ]
      ; select_measure
      ; Node.pre [ Node.text (Sexp.to_string_hum [%sexp (column : string option)]) ]
      ; select_column
      ])
;;

let view ~env =
  let%sub mdx =
    let%arr env = env in
    (module Mdx.Make ((val env : Mdx.Env.S)) : Mdx.S)
  in
  frm mdx
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
(* (Bonsai.const chartjs) *)
