open Se
open Symbolic
open Expr

(** Converts an expression to a C template-compatible string. *)
let rec c_template_repr out (e : t) =
  match e with
  | Uop (Qasm.INV, Uop (Qasm.SQRT, Cst 2.)) ->
    Printf.fprintf out "SQRT1_2"
  | Bop (op, e1, e2) ->
    Printf.fprintf out "%s(%a,%a)" (cstring_of_binary op) c_template_repr e1 c_template_repr e2
  | Uop (op, e) ->
    Printf.fprintf out "%s(%a)" (cstring_of_unary op) c_template_repr e
  | Cst c -> Printf.fprintf out "(cfloat){%f,0}" c
  | Pi -> Printf.fprintf out "(cfloat){M_PI,0}"
  | I -> Printf.fprintf out "(cfloat){0,1}"
  | Var v -> Printf.fprintf out "state[%d]" v
  | CustomSymbol s -> Printf.fprintf out "%s" s

let c_egraph_template_repr out = 
  List.iter (fun (i, e) -> 
      Printf.fprintf out "\tconst cfloat x_%d = %a;\n" i c_template_repr e)
    (Egraph.repr_all ())

(** C code for the evaluation of one state *)
let emit_state out i s =
  Printf.fprintf out "out_state[%d] = %a;\n\t" i c_template_repr s

(** Returns a string representing all the updates to the state vector
    resulting from the circuit (the final reduced expressions).
*)
let emit se_state out =
  se_state
  |> Array.iteri (emit_state out)

let emit_egraph_state out i s =
  Printf.fprintf out "out_state[%d] = x_%d;\n\t" i s

let emit_egraph se_egraph_state out = 
  c_egraph_template_repr out;
  (* Emit the final values *)
  se_egraph_state
  |> Array.iteri (emit_egraph_state out)

(** Emits C code for a given quantum circuit to the provided output channel. *)
let emitc out circ =
  let se_res = SE_Engine.run circ in

  let number_of_states = Int.to_string (Array.length se_res) in

  let template_lines = Utils.lines "src/c_template.c" in

  List.iter (fun line ->
      if line = "#define N {{N}}" then
        Printf.fprintf out "#define N %s\n" number_of_states
      else if String.trim line = "{{SymbolicExpressions}}" then
        (emit se_res out)
      else Printf.fprintf out "%s\n" line
    ) template_lines;

  flush out

let emitc_egraph out circ =
  let res = SE_Egraph_Engine.run circ in

  let number_of_states = Int.to_string (Array.length res) in

  let template_lines = Utils.lines "src/c_template.c" in

  List.iter (fun line ->
      if line = "#define N {{N}}" then
        Printf.fprintf out "#define N %s\n" number_of_states
      else if String.trim line = "{{SymbolicExpressions}}" then
        (emit_egraph res out)
      else Printf.fprintf out "%s\n" line
    ) template_lines;

  flush out
