module type ARRANGEMENT = sig
  type name
  type state
  type input
  type output
  type msg
  type client_id
  type res = (output list * state) * ((name * msg) list)
  val systemName : string
  val init : name -> state
  val reboot : state -> state
  val handleIO : name -> input -> state -> res
  val handleNet : name -> name -> msg -> state -> res
  val handleTimeout : name -> state -> res
  val setTimeout : name -> state -> float
  val deserializeMsg : string -> msg
  val serializeMsg : msg -> string
  val deserializeInput : string -> client_id -> input option
  val serializeOutput : output -> client_id * string
  val debug : bool
  val debugInput : state -> input -> unit
  val debugRecv : state -> (name * msg) -> unit
  val debugSend : state -> (name * msg) -> unit
  val debugTimeout : state -> unit
  val createClientId : unit -> client_id
  val serializeClientId : client_id -> string
end

module Shim :
  functor (A : ARRANGEMENT) ->
    sig
      type cfg = {
        cluster : (A.name * (string * int)) list;
        me : A.name;
        port : int;
        dbpath : string;
      }
      val main : cfg -> unit
    end
