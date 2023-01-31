(** C Code emission.
    This module implements C code generation for quantum circuits (see {!Circuit.t}).
    It uses a symbolic version of the simulator (see {!Symbolic} and {!Simulation}).
*)

(** [emitc circ out] is generating
    C code for the simulation of circuit [circ]
    in the given output channel [out].
*)
val emitc : out_channel -> Circuit.t -> unit


val emitc_egraph : out_channel -> Circuit.t -> unit