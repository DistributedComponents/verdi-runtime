open Printf
open Util
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

module Shim (A: ARRANGEMENT) = struct
  type cfg = { me : A.name
             ; debugger_addr : (string * int)
             }
  
  type env =
      { cfg : cfg
      ; debugger_fd : Unix.file_descr
      }

  (* Load state from disk, initialize environment, and start server. *)
  let setup (cfg : cfg) : (env * A.state) =
    Random.self_init ();
    let env = { cfg = cfg
              ; debugger_fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
              } in
    Unix.setsockopt env.debugger_fd Unix.SO_REUSEADDR true;
    Unix.setsockopt env.debugger_fd Unix.TCP_NODELAY true;
    let hostname = (Unix.gethostbyname (fst cfg.debugger_addr)).Unix.h_addr_list.(0) in
    let addr = Unix.ADDR_INET(hostname, snd cfg.debugger_addr) in
    Unix.connect env.debugger_fd addr;
    (env, A.init env.cfg.me)

  let respond env (((ops, os), s), ps) =
    (* TODO: serialize response *)
    s

  let send_json env (j : json) =
    let js = to_string j in
    send_chunk env.debugger_fd (Bytes.of_string js)

  let recv_json env =
    let js = recv_full_chunk env.debugger_fd in
    from_string (Bytes.to_string js)

  let register (env : env) : unit =
    let registration_json =
      `Assoc([("msgtype", `String("register"))
             ; ("name", `String(A.string_of_name env.cfg.me))]) in
    send_json env registration_json

  let get_field json f =
    let rec get_field_lst l =
      match l with
      | [] -> raise Not_found
      | (k, v) :: l' -> if f == k then v else get_field_lst l'
    in
    match json with
    | `Assoc l -> get_field_lst l
    | _ -> raise Not_found

  let response_message src (dst, message) =
    `Assoc [("from", `String(A.string_of_name src))
          ; ("to", `String(A.string_of_name dst))
          ; ("type", `String("Msg"))
          ; ("body", A.json_of_msg message)]
    
  let response name state messages ?(set_timeout=false) () =
    `Assoc [("state", A.json_of_state state)
          ; ("messages", `List (List.map (response_message name) messages))
          ; ("set-timeouts",
             if set_timeout then
               `List [`Assoc [("to", `String(A.string_of_name name))
                            ; ("type", `String("Timeout"))
                            ; ("body", `String("Timeout!"))]]
             else `List [])]
                
  let handle_debugger_msg me state msg =
    let t = get_field msg "msgtype" in
    match t with
    | `String "start" ->
       (A.init me, response me (A.init me) [] ())
    | _ -> raise Not_found
    
  let rec eloop (env : env) (state : A.state) : unit =
    let j = recv_json env in
    let (state, resp) = handle_debugger_msg env.cfg.me state j in
    eloop env state
    
  let main (cfg : cfg) : unit =
    printf "debug shim running setup for %s" A.system_name;
    print_newline ();
    let (env, initial_state) = setup cfg in
    print_endline "debug shim ready for action";
    eloop env initial_state
end
