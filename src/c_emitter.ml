open Se
open Symbolic

(* The CEmitter uses a symbex backend to evaluate a circuit and outputs pure C code based on a template. *)
module CEmitter : sig
  val emit  : SBackend.qstate -> string
  (* val process_template : string -> string -> SBackend.qstate -> unit *)

  val emitc : Circuit.t -> out_channel -> unit

end = struct
  (* Returns a string representing all the updates to the state vector resulting from the circuit (the final reduced expressions). *)
  let emit se_state =  
    String.concat "" Array.(to_list (mapi (fun i s -> (Printf.sprintf "out_state[%d] = %s;\n\t" i (Expr.c_template_repr s))) se_state))
      
  (* Emits C code for a given quantum circuit to the provided output channel. *)
  let emitc circ out = 
    let se_res = SE_Engine.run circ in
    let lines file =
      let contents = In_channel.with_open_bin file In_channel.input_all in
      String.split_on_char '\n' contents 
    in
    
    let number_of_states = Int.to_string (Array.length se_res) in
    
    let template_lines = lines "src/c_template.c" in
    
    (* TODO: Rewrite this to be more maintainable *)
    let output_lines = List.map (fun line ->
      Str.global_replace
        (Str.regexp "{{N}}")
        number_of_states
        (Str.global_replace (Str.regexp "{{SymbolicExpressions}}") (emit se_res) line)
    ) template_lines 
    in
    
    output_string out (String.concat "\n" output_lines);
    flush out
end