type ('env, 'state) task  = 
  { fd : Unix.file_descr
  ; mutable select_on : bool
  ; mutable wake_time : float option
  ; mutable process_read : ('env, 'state) task -> 'env -> 'state -> bool * ('env, 'state) task list * 'state
  ; mutable process_wake : ('env, 'state) task -> 'env -> 'state -> bool * ('env, 'state) task list * 'state
  ; finalize : ('env, 'state) task -> 'env -> 'state -> 'state
  }

val eloop : float -> float -> (Unix.file_descr, ('a, 'b) task) Hashtbl.t -> 'a -> 'b -> 'c