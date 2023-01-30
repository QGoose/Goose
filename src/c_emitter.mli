(** C Code emission.
    This module implements C code generation for quantum circuits (see {!Circuit.t}).
    It uses a symbolic version of the simulator (see {!Symbolic} and {!Simulation}).
*)

(** [emitc circ out] is generating
    C code for the simulation of circuit [circ]
    in the given output channel [out].
*)
val emitc : Circuit.t -> out_channel -> unit


val emitc_egraph : Circuit.t -> out_channel -> unit