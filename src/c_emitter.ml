open Se
open Symbolic
open Expr

(** Converts an expression to a C template-compatible string. *)
let rec c_template_repr (e : t) =
  match reduce (reduce e) with
  | Uop (Qasm.INV, Uop (Qasm.SQRT, Cst 2)) ->
    Printf.sprintf "SQRT1_2"
  | Bop (op, e1, e2) ->
    Printf.sprintf "%s(%s,%s)" (cstring_of_binary op) (c_template_repr e1) (c_template_repr e2)
  | Uop (op, e) ->
    Printf.sprintf "%s(%s)" (cstring_of_unary op) (c_template_repr e)
  | Cst c -> Printf.sprintf "(cfloat){%d,0}" c
  | Pi -> Printf.sprintf "(cfloat){M_PI,0}"
  | I -> Printf.sprintf "(cfloat){0,1}"
  | Var v -> Printf.sprintf "state[%d]" (Symbol.index v)
  | CustomSymbol s -> s

(** C code for the evaluation of one state *)
let emit_state i s =
  Printf.sprintf "out_state[%d] = %s;\n\t" i (c_template_repr s)

(** Returns a string representing all the updates to the state vector
    resulting from the circuit (the final reduced expressions).
*)
let emit se_state =
  se_state
  |> Array.mapi emit_state
  |> Array.to_list
  |> String.concat ""

(** Emits C code for a given quantum circuit to the provided output channel. *)
let emitc circ out =
  let se_res = SE_Engine.run circ in
  let lines file =
    In_channel.with_open_bin file In_channel.input_all
    |> String.split_on_char '\n'
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