open Se
open Symbolic

(* The CEmitter uses a symbex backend to evaluate a circuit and outputs pure C code based on a template. *)
module CEmitter : sig
  val emit  : SBackend.qstate -> string
  val process_template : string -> string -> SBackend.qstate -> unit

end = struct
  (* Returns a string representing all the updates to the state vector resulting from the circuit (the final reduced expressions). *)
  let emit se_state =  
    String.concat "" Array.(to_list (mapi (fun i s -> (Printf.sprintf "out_state[%d] = %s;\n\t" i (Expr.c_template_repr s))) se_state))

  (* TODO: Rewrite this to be more readable and maintainable *)
  (* Processes a template file and outputs the C code to the given file by emitting the update code of the given symbolic state vector. *)
  let process_template template_file output_file se_state =
    let lines file =
      let contents = In_channel.with_open_bin file In_channel.input_all in
      String.split_on_char '\n' contents in
    let write_lines file lines =
      let output_lines oc = output_string oc (String.concat "\n" lines) in
      Out_channel.with_open_bin file output_lines in
    let number_of_states = Int.to_string (Array.length se_state) in
    

    let template_lines = lines template_file in
    let output_lines = List.map (fun line -> Str.global_replace (Str.regexp "{{N}}") number_of_states (Str.global_replace (Str.regexp "{{SymbolicExpressions}}") (emit se_state) line)) template_lines in
    write_lines output_file output_lines 

end