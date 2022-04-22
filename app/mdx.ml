open Core

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

  let equal (module Env1 : S) (module Env2 : S) =
    String.(
      Env1.host = Env2.host
      && Env1.ns = Env2.ns
      && Env1.user = Env2.user
      && Env1.pass = Env2.pass)
    && Env1.port = Env2.port
  ;;

  open Sexp

  let to_sexp (module Env : S) =
    List
      [ List [ Atom "host"; Atom Env.host ]
      ; List [ Atom "port"; Atom (Int.to_string Env.port) ]
      ; List [ Atom "ns"; Atom Env.ns ]
      ; List [ Atom "user"; Atom Env.user ]
      ; List [ Atom "pass"; Atom Env.pass ]
      ]
  ;;

  let of_sexp = function
    | List
        [ List [ Atom "host"; Atom host ]
        ; List [ Atom "port"; Atom port ]
        ; List [ Atom "ns"; Atom ns ]
        ; List [ Atom "user"; Atom user ]
        ; List [ Atom "pass"; Atom pass ]
        ] -> make ~host ~port:(Int.of_string port) ~ns ~user ~pass
    | _ -> assert false
  ;;
end

module type S = sig
  type http_error =
    [ `Unauthorized
    | `OtherStatus of Cohttp.Code.status_code
    ]
  [@@deriving sexp]

  type data_error = [ `WrondData of Yojson.Safe.t ] [@@deriving sexp]

  module MdxResponse : sig
    type tuple =
      { caption : string
      ; dimension : string
      ; path : string
      }
    [@@deriving sexp]

    type col = { tuples : tuple list } [@@deriving sexp]

    type info =
      { colCount : int
      ; colKey : string
      ; cubeClass : string
      ; cubeKey : string
      ; cubeName : string
      ; decimalSeparator : string
      ; numericGroupSeparator : string
      ; numericGroupSize : int
      ; percentDone : int
      ; queryKey : string
      ; rowCount : int
      ; rowKey : string
      }
    [@@deriving sexp]

    type t =
      { cols : col * col
      ; data : float option list
      ; info : info
      }
    [@@deriving sexp]
  end

  val mdx : string -> (MdxResponse.t, [ http_error | data_error ]) Result.t Lwt.t
  val test : unit -> (unit, http_error) Result.t Lwt.t
end

module Make (Env : Env.S) : S = struct
  type http_error =
    [ `Unauthorized
    | `OtherStatus of Cohttp.Code.status_code
    ]
  [@@deriving sexp]

  type data_error = [ `WrondData of Yojson_sexp.Safe.t ] [@@deriving sexp]

  let make_url ?query =
    let query = Option.value ~default:[] query in
    Uri.make ~host:Env.host ~port:Env.port ~query:(("Namespace", [ Env.ns ]) :: query)
  ;;

  let auth_headers =
    Cohttp.Header.add_authorization (Cohttp.Header.init ()) (`Basic (Env.user, Env.pass))
  ;;

  open Lwt.Let_syntax

  let test () =
    let%bind r, _ =
      make_url ~path:"MDX2JSON/Test" ()
      |> Cohttp_lwt_jsoo.Client.get ~headers:auth_headers
    in
    match Cohttp.Response.status r with
    | `OK -> Lwt.return (Ok ())
    | `Unauthorized -> Lwt.return (Error `Unauthorized)
    | status -> Lwt.return (Error (`OtherStatus status))
  ;;

  module MdxResponse = struct
    type tuple =
      { caption : string
      ; dimension : string
      ; path : string
      }
    [@@deriving sexp]

    type col = { tuples : tuple list } [@@deriving sexp]

    type info =
      { colCount : int
      ; colKey : string
      ; cubeClass : string
      ; cubeKey : string
      ; cubeName : string
      ; decimalSeparator : string
      ; numericGroupSeparator : string
      ; numericGroupSize : int
      ; percentDone : int
      ; queryKey : string
      ; rowCount : int
      ; rowKey : string
      }
    [@@deriving sexp]

    type t =
      { cols : col * col
      ; data : float option list
      ; info : info
      }
    [@@deriving sexp]
  end

  let mdx query =
    let%bind r, b =
      make_url ~path:"MDX2JSON/MDX" ()
      |> Cohttp_lwt_jsoo.Client.post
           ~body:
             (`Assoc [ "MDX", `String query ]
             |> Yojson.Safe.to_string
             |> Cohttp_lwt.Body.of_string)
           ~headers:auth_headers
    in
    match Cohttp.Response.status r with
    | `OK ->
      let%bind b = Cohttp_lwt.Body.to_string b in
      let j = Yojson.Safe.from_string b in
      let m =
        match j with
        | `Assoc
            [ ( "Cols"
              , `List
                  [ `Assoc [ ("tuples", `List tuples1) ]
                  ; `Assoc [ ("tuples", `List tuples2) ]
                  ] )
            ; ("Data", `List data)
            ; ( "Info"
              , `Assoc
                  [ ("colCount", `Int colCount)
                  ; ("colKey", `String colKey)
                  ; ("cubeClass", `String cubeClass)
                  ; ("cubeKey", `String cubeKey)
                  ; ("cubeName", `String cubeName)
                  ; ("decimalSeparator", `String decimalSeparator)
                  ; ("numericGroupSeparator", `String numericGroupSeparator)
                  ; ("numericGroupSize", `Int numericGroupSize)
                  ; ("percentDone", `Int percentDone)
                  ; ("queryKey", `String queryKey)
                  ; ("rowCount", `Int rowCount)
                  ; ("rowKey", `String rowKey)
                  ] )
            ] ->
          let to_tuples tuples =
            List.fold_result tuples1 ~init:[] ~f:(fun acc -> function
              | `Assoc l ->
                begin
                  match
                    let%bind.Option caption =
                      List.Assoc.find ~equal:String.equal l "caption"
                    in
                    let%bind.Option dimension =
                      List.Assoc.find ~equal:String.equal l "dimension"
                    in
                    let%bind.Option path = List.Assoc.find ~equal:String.equal l "path" in
                    match caption, dimension, path with
                    | `String caption, `String dimension, `String path ->
                      Some MdxResponse.{ caption; dimension; path }
                    | _ -> None
                  with
                  | Some c -> Ok (c :: acc)
                  | None -> Error (`WrondData (`List tuples))
                end
              | item -> Error (`WrondData item))
            |> Result.map ~f:List.rev
          in
          let%bind.Result tuples1 = to_tuples tuples1 in
          let%bind.Result tuples2 = to_tuples tuples2 in
          let%bind.Result data =
            List.fold_result data ~init:[] ~f:(fun acc -> function
              | `Int i -> Ok (Some (Int.to_float i) :: acc)
              | `Float f -> Ok (Some f :: acc)
              | `String "" -> Ok (None :: acc)
              | item -> Error (`WrondData item))
            |> Result.map ~f:List.rev
          in
          let info =
            MdxResponse.
              { colCount
              ; colKey
              ; cubeClass
              ; cubeKey
              ; cubeName
              ; decimalSeparator
              ; numericGroupSeparator
              ; numericGroupSize
              ; percentDone
              ; queryKey
              ; rowCount
              ; rowKey
              }
          in
          Ok MdxResponse.{ cols = { tuples = tuples1 }, { tuples = tuples2 }; data; info }
        | data ->
          Log.yojson data;
          Error (`WrondData data)
      in
      Lwt.return m
    | `Unauthorized -> Lwt.return (Error `Unauthorized)
    | status -> Lwt.return (Error (`OtherStatus status))
  ;;
end
