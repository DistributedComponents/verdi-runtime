module type ARRANGEMENT = sig
  type name
  type state
  type input
  type output
  type msg
  type client_id
  type res = (output list * state) * ((name * msg) list)
  type task_handler = name -> state -> res
  type timeout_setter = name -> state -> float option
  val systemName : string
  val serializeName : name -> string
  val deserializeName : string -> name option
  val init : name -> state
  val handleIO : name -> input -> state -> res
  val handleNet : name -> name -> msg -> state -> res
  val deserializeMsg : string -> msg
  val serializeMsg : msg -> string
  val deserializeInput : string -> client_id -> input option
  val serializeOutput : output -> client_id * string
  val debug : bool
  val debugInput : state -> input -> unit
  val debugRecv : state -> (name * msg) -> unit
  val debugSend : state -> (name * msg) -> unit
  val createClientId : unit -> client_id
  val serializeClientId : client_id -> string
  val timeoutTasks : (task_handler * timeout_setter) list
end

module Shim :
  functor (A : ARRANGEMENT) ->
    sig
      type cfg = 
	{ cluster : (A.name * (string * int)) list
	; me : A.name
	; port : int
	}
      val main : cfg -> unit
    end
