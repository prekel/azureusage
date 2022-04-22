open Core

type data_dimension =
  [ `ServiceName
  | `ServiceType
  | `ServiceResource
  | `ResourceGuid
  | `ServiceRegion
  | `SubscriptionName
  | `SubscriptionGuid
  ]
[@@deriving sexp, equal]

let all_data_dimensions =
  [ `ServiceName
  ; `ServiceType
  ; `ServiceResource
  ; `ResourceGuid
  ; `ServiceRegion
  ; `SubscriptionName
  ; `SubscriptionGuid
  ]
;;

type time_dimension =
  [ `Year
  | `Month
  | `Day
  ]
[@@deriving sexp, equal]

let all_time_dimensions = [ `Year; `Month; `Day ]

type measure =
  [ `Count
  | `Cost
  | `Quantity
  | `Price
  ]
[@@deriving sexp, equal]

let all_measures = [ `Count; `Cost; `Quantity; `Price ]

let path_prefix = function
  | `ServiceName -> "[Service].[H1].[ServiceName]"
  | `ServiceType -> "[Service].[H1].[ServiceType]"
  | `ServiceResource -> "[Service].[H1].[ServiceResource]"
  | `ResourceGuid -> "[Service].[H1].[ResourceGuid]"
  | `ServiceRegion -> "[ServiceRegion].[H1].[ServiceRegion]"
  | `SubscriptionName -> "[Subscription].[H1].[SubscriptionName]"
  | `SubscriptionGuid -> "[Subscription].[H1].[SubscriptionGuid]"
  | `Count -> {|[Measures].[%COUNT]|}
  | `Cost -> "[Measures].[Cost]"
  | `Quantity -> "[Measures].[Quantity]"
  | `Price -> "[Measures].[Price]"
  | `Year -> "[Date].[H1].[Year]"
  | `Month -> "[Date].[H1].[Month]"
  | `Day -> "[Date].[H1].[Day]"
;;

let path_value ~(dimension : data_dimension) value =
  path_prefix dimension ^ ".&[" ^ value ^ "]"
;;

let path_members ~(dimension : [< data_dimension | time_dimension ]) =
  path_prefix dimension ^ ".Members"
;;

(* SELECT NON EMPTY
   NONEMPTYCROSSJOIN([Service].[H1].[ServiceName].Members,[Measures].[Cost]) ON 0,NON
   EMPTY [Date].[H1].[Day].Members ON 1 FROM [AZUREUSAGE] %FILTER
   %OR({[SERVICE].[H1].[SERVICENAME].&[Azure App
   Service],[SERVICE].[H1].[SERVICENAME].&[Backup],[SERVICE].[H1].[SERVICENAME].&[Container
   Instances],[SERVICE].[H1].[SERVICENAME].&[Storage]}) *)
let query_by_time
    ~(data : data_dimension)
    ~(time : time_dimension)
    ~(measure : measure)
    filter
  =
  sprintf
    {|
      SELECT NON EMPTY 
      NONEMPTYCROSSJOIN(%s,%s) 
      ON 0,NON EMPTY %s ON 1 FROM [AZUREUSAGE] 
      %%FILTER %%OR({%s})|}
    (path_members ~dimension:data)
    (path_prefix measure)
    (path_members ~dimension:time)
    (filter |> List.map ~f:(path_value ~dimension:data) |> String.concat ~sep:",")
;;

(* SELECT NON EMPTY [Service].[H1].[ServiceName].Members ON 0 FROM [AZUREUSAGE] *)
let query_columns_by_dimension ~(data : data_dimension) =
  sprintf {|SELECT NON EMPTY %s ON 0 FROM [AZUREUSAGE]|} (path_members ~dimension:data)
;;

let columns_by_dimension Mdx.MdxResponse.{ cols = { tuples }, _; _ } =
  List.map tuples ~f:(fun t -> t.caption)
;;

let by_time
    Mdx.MdxResponse.
      { cols = { tuples = columns }, dates
      ; data
      ; info = { rowCount = column_count; colCount = date_count; _ }
      }
  =
  let dates =
    match dates with
    | Some { tuples } ->
      tuples |> List.map ~f:(fun t -> t.valueID, t.caption) |> List.to_array
    | None -> assert false
  in
  let columns = columns |> List.map ~f:(fun t -> t.caption) |> List.to_array in
  data
  |> List.foldi
       ~init:(Array.make_matrix ~dimx:column_count ~dimy:date_count None)
       ~f:(fun i acc el ->
         acc.(i mod column_count).(i mod date_count) <- el;
         acc)
  |> Array.transpose_exn
  |> Array.to_list
  |> List.map ~f:(fun a -> a |> Array.to_list |> List.mapi ~f:(fun i t -> dates.(i), t))
  |> List.mapi ~f:(fun i a -> columns.(i), a)
;;
