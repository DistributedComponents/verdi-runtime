open Yojson.Basic

module type ARRANGEMENT = sig
  type name
  type state
  type input
  type output
  type msg
  type res = (output list * state) * ((name * msg) list)
  val system_name : string
  val init : name -> state
  val handle_input : name -> input -> state -> res
  val handle_msg : name -> name -> msg -> state -> res
  val handle_timeout : name -> state -> res
  val deserialize_msg : bytes -> msg
  val serialize_msg : msg -> bytes
  val deserialize_input : bytes -> input option
  val commands : string list
  val string_of_name : name -> string
  val name_of_string : string -> name
  val type_of_msg : msg -> string
  val json_of_msg : msg -> json
  val json_of_state : state -> json
end

module Shim :
  functor (A : ARRANGEMENT) ->
    sig
      type cfg =
        { me : A.name
        ; debugger_addr : (string * int)
        }
      val main : cfg -> unit
    end
