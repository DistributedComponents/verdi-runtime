open Printf
open Util
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

module Shim (A: ARRANGEMENT) = struct
  type cfg = { me : A.name
             ; debugger_addr : (string * int)
             }
  
  type env =
      { cfg : cfg
      ; debugger_fd : Unix.file_descr
      }

  let setup (cfg : cfg) : (env * A.state) =
    Random.self_init ();
    let env = { cfg = cfg
              ; debugger_fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
              } in
    Unix.setsockopt env.debugger_fd Unix.SO_REUSEADDR true;
    let hostname = (Unix.gethostbyname (fst cfg.debugger_addr)).Unix.h_addr_list.(0) in
    let addr = Unix.ADDR_INET(hostname, snd cfg.debugger_addr) in
    Unix.connect env.debugger_fd addr;
    (env, A.init env.cfg.me)

  let send_json env (j : json) =
    let js = to_string j in
    Printf.printf "Sending json: %s\n" js;
    send_chunk_big_endian env.debugger_fd (Bytes.of_string js)

  let recv_json env =
    let js = recv_full_chunk_big_endian env.debugger_fd in
    Printf.printf "Got json: %s\n" (Bytes.to_string js);
    from_string (Bytes.to_string js)

  let get_field json f =
    let rec get_field_lst l =
      match l with
      | [] -> raise Not_found
      | (k, v) :: l' -> if f = k then v else get_field_lst l'
    in
    match json with
    | `Assoc l -> get_field_lst l
    | _ -> raise Not_found

  let get_bool_field json f =
    match get_field json f with
    | `Bool v -> v
    | _ -> raise Not_found

  let get_string_field json f =
    match get_field json f with
    | `String v -> v
    | _ -> raise Not_found
         
  exception CommunicationError of string
         
  let register (env : env) : unit =
    let registration_json =
      `Assoc([("msgtype", `String("register"))
             ; ("name", `String(A.string_of_name env.cfg.me))]) in
    send_json env registration_json;
    let resp = recv_json env in
    let ok = try
        get_bool_field resp "ok"
      with
      | Not_found -> false
    in
    if not ok then raise (CommunicationError "Registration failed")


  let response_message src (dst, message) =
    `Assoc [("from", `String(A.string_of_name src))
          ; ("to", `String(A.string_of_name dst))
          ; ("type", `String(A.type_of_msg message))
          ; ("body", A.json_of_msg message)
          ; ("raw", `String(B64.encode (Bytes.to_string (A.serialize_msg message))))]

  let command_timeouts name =
    List.map (fun c -> `Assoc [("to", `String(A.string_of_name name))
                             ; ("type", `String("Command"))
                             ; ("body", `String(c))]) A.commands
    
  let response name state messages ?(set_timeout=false) () =
    `Assoc [("states", `Assoc [(A.string_of_name name, A.json_of_state state)])
          ; ("send-messages", `List (List.map (response_message name) messages))
          ; ("set-timeouts",
             if set_timeout then
               `List ((`Assoc [("to", `String(A.string_of_name name))
                            ; ("type", `String("Timeout"))
                            ; ("body", `String("Timeout!"))])
                      :: command_timeouts name)
             else `List [])]
                
  let handle_debugger_msg me state msg =
    let t = get_field msg "msgtype" in
    match t with
    | `String "start" ->
       (A.init me, response me (A.init me) [] ~set_timeout:true ())
    | `String "timeout" ->
       (* ignore output for now... *)
       (match get_string_field msg "type" with
        | "Command" ->
           let body = get_string_field msg "body" in
           let inp = A.deserialize_input (Bytes.of_string body) in
           (match inp with
            | None -> (state, response me state [] ())
            | Some input ->
               let ((_, s), msgs) = A.handle_input me input state in
               (s, response me s msgs ~set_timeout:false ()))
        | _ ->
           let ((_, s), msgs) = A.handle_timeout me state in
           (s, response me s msgs ~set_timeout:false ()))
    | `String "msg" ->
       let from = A.name_of_string (get_string_field msg "from") in
       let body = get_string_field msg "raw" in
       let msg = A.deserialize_msg (Bytes.of_string (B64.decode body)) in
       let ((_, s), msgs) = A.handle_msg me from msg state in
       (s, response me s msgs ~set_timeout:false ())
    | `String "command" ->
       let inp = A.deserialize_input (Bytes.of_string (get_string_field msg "command")) in
       (match inp with
        | None -> (state, response me state [] ())
        | Some input ->
           let ((_, s), msgs) = A.handle_input me input state in
           (s, response me s msgs ~set_timeout:false ()))
    | _ -> raise Not_found
    
  let rec eloop (env : env) (state : A.state) : unit =
    let j = recv_json env in
    let (state, resp) = handle_debugger_msg env.cfg.me state j in
    send_json env resp;
    flush stdout;
    eloop env state
    
  let main (cfg : cfg) : unit =
    printf "debug shim running setup for %s" A.system_name;
    print_newline ();
    let (env, initial_state) = setup cfg in
    print_endline "debug shim ready for action";
    register env;
    print_endline "registered";
    eloop env initial_state
end
