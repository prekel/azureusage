open Core

module LexingPosition = struct
  type t = Lexing.position =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving sexp]
end

module Yojson = struct
  module Safe = struct
    type t =
      [ `Null
      | `Bool of bool
      | `Int of int
      | `Intlit of string
      | `Float of float
      | `String of string
      | `Assoc of (string * t) list
      | `List of t list
      | `Tuple of t list
      | `Variant of string * t option
      ]
    [@@deriving sexp, equal]
  end
end
