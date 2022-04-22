open Js_of_ocaml

module JSON = struct
  open Js

  class type json =
    object
      method parse : 'a. js_string t -> 'a meth
    end

  let json : json Js.t = Unsafe.global##._JSON
end

let yojson j =
  let s = j |> Yojson.Safe.to_string |> Js.string in
  let o = JSON.json##parse s in
  Firebug.console##log o
;;
