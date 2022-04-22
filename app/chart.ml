open Core 
open Js_of_ocaml

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

module Widget = struct
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
