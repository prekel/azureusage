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

  val mdx : string -> (float option list, [ http_error | data_error ]) Result.t Lwt.t
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
      ; cellStyle : string
      ; dimension : string
      ; format : string
      ; headerStyle : string
      ; path : string
      ; title : string
      ; total : string
      ; type_ : string
      ; valueID : int
      ; vis : int
      }
    [@@deriving sexp]

    type col = { tuples : tuple list } [@@deriving sexp]

    type info =
      { colCount : string
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
      { cols : col list
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
        | `Assoc [ ("Cols", _); ("Data", `List list); ("Info", _) ] ->
          List.fold_result list ~init:[] ~f:(fun acc -> function
            | `Int i -> Ok (Some (Int.to_float i) :: acc)
            | `Float f -> Ok (Some f :: acc)
            | `String "" -> Ok (None :: acc)
            | item -> Error (`WrondData item))
          |> Result.map ~f:List.rev
        | data ->
          Log.yojson data;
          Error (`WrondData data)
      in
      Lwt.return m
    | `Unauthorized -> Lwt.return (Error `Unauthorized)
    | status -> Lwt.return (Error (`OtherStatus status))
  ;;
end
