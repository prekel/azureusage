open Js_of_ocaml
open Lwt.Let_syntax

let () =
  Lwt.async (fun () ->
      let module Mdx =
        Mdx.Make (struct
          let host = "10.49.0.1"
          let port = 9092
          let ns = "AZUREUSAGE"
          let user = "_SYSTEM"
          let pass = "886nZB7xUw7GMXQ"
        end)
      in
      let query =
        {|SELECT NON EMPTY NONEMPTYCROSSJOIN([Date].[H1].[Month].Members,[Measures].[Cost]) ON 0,NON EMPTY [ServiceRegion].[H1].[ServiceRegion].Members ON 1 FROM [AZUREUSAGE]|}
      in
      let%bind _r, b = Mdx.mdx query in
      let%bind b = Cohttp_lwt.Body.to_string b in
      Firebug.console##log (b |> Js.string);
      Lwt.return_unit)
;;
