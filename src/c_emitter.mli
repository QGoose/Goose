(** [emitc circ out] is generating
    C code for the simulation of circuit [circ]
    in the given output channel [out].
*)
val emitc : Circuit.t -> out_channel -> unit