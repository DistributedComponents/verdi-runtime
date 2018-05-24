open Yojson.Basic

module type ARRANGEMENT = sig
  type name
  type state
  type input
  type output
  type msg
  type client_id
  type res = (output list * state) * ((name * msg) list)
  val system_name : string
  val init : name -> state
  val reboot : state -> state
  val handle_input : name -> input -> state -> res
  val handle_msg : name -> name -> msg -> state -> res
  val handle_timeout : name -> state -> res
  val deserialize_msg : bytes -> msg
  val serialize_msg : msg -> bytes
  val deserialize_input : bytes -> client_id -> input option
  val serialize_output : output -> client_id * bytes
  val debug : bool
  val debug_input : state -> input -> unit
  val debug_recv : state -> (name * msg) -> unit
  val debug_send : state -> (name * msg) -> unit
  val debug_timeout : state -> unit
  val string_of_client_id : client_id -> string
  val string_of_name : name -> string
  val json_of_msg : msg -> json
  val msg_of_json : json -> msg
  val json_of_state : state -> json
  val state_of_json : json -> state
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
