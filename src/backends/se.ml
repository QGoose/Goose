open Symbolic
open Simulation

(**
  A Symbolic backend to simulate quantum circuits
*)
module SBackend = struct
  type qstate = Expr.t array
  
  let iteration_indices (i : int) (t : int) : int * int =
    let mask = (1 lsl t) - 1 in
    let notMask = lnot mask in
    let i1 = (i land mask) lor ((i land notMask) lsl 1) in
    let i2 = i1 lor (1 lsl t) in
    (i1, i2)

  let init qbits =
    let len = 1 lsl qbits in
    (* All-zero state *)
    let state = Array.init len (fun _ -> Expr.Var (Symbol.fresh ())) in
    (* let state = Array.make len (Expr.Cst (Complex.zero)) in *)
    (* effectfully set the |00...0> entry to 1 *)
    (* Array.set state 0 (Expr.Cst (Complex.one)); *)
    state

  let controls_check (state_index: int) (controls: Circuit.adr list): bool =
    let check (Circuit.A c) = (1 lsl c) land state_index > 0 in
    List.(fold_left (&&) true (map check controls))

  type matrix = Expr.t * Expr.t * Expr.t * Expr.t

  let cpx_inv_sqrt_2 =
    Expr.Uop (INV, Uop (SQRT, Cst 2))

  let cpx_2_pi =
    Expr.(Cst 2 *! Pi)

  let cpx_pow_2 m =
    Expr.Bop (POW, Cst 2, Cst m)

  let cpx_omega m =
    Expr.(Uop (EXP, Bop (DIV, cpx_2_pi *! I, cpx_pow_2 m)))

    
  let matrix_for_gate (g : Circuit.gate_kind) : matrix =
    match g with
    | X -> Expr.(Cst 0, Cst 1, Cst 1, Cst 0)
    | Z -> Expr.(Cst 1, Cst 0, Cst 0, Cst (-1))
    | H -> Expr.(cpx_inv_sqrt_2, cpx_inv_sqrt_2, cpx_inv_sqrt_2, neg cpx_inv_sqrt_2)
    | Rm m -> Expr.(Cst 0, Cst 0, Cst 0, cpx_omega m)

  let apply_gate (g : Circuit.gate) (state : qstate) = 
    let iterations = Array.length state / 2 in
    let (a, b, c, d) = matrix_for_gate g.kind in
    let (A t) = g.target in

    for i = 0 to iterations - 1 do
      let (i1, i2) = iteration_indices i t in
      let c1 = state.(i1) in
      let c2 = state.(i2) in
      let check = controls_check i1 g.controls in
      if check then begin
        state.(i1) <- Expr.((a *! c1) +! (b *! c2));
        state.(i2) <- Expr.((c *! c1) +! (d *! c2));
      end
    done

  let repr x = x
end

(** Symbolic Simulator *)
module SE_Engine = Make(SBackend)
