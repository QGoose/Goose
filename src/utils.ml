let todo (s : string) = Printf.sprintf "unimplemented: %s" s |> failwith

let int2bin n bits =
  let buf = Bytes.create bits in
  for i = 0 to bits - 1 do
    let pos = bits - 1 - i in
    Bytes.set buf pos (if n land (1 lsl i) != 0 then '1' else '0')
  done;
  Bytes.to_string buf

let rec log2 x =
  if x <= 1 then 0
  else 1 + log2 (x lsr 1)

(** Returns the indices of the state amplitudes required for the ith iteration of a gate on target t. *)
let iteration_indices (i : int) (t : int) : int * int =
  let mask = (1 lsl t) - 1 in
  let notMask = lnot mask in
  let i1 = (i land mask) lor ((i land notMask) lsl 1) in
  let i2 = i1 lor (1 lsl t) in
  (i1, i2)

(** Check if an iteration should execute based on the controls of the gate. *)
let controls_check (state_index: int) (controls: Circuit.adr list): bool =
  let check (Circuit.A c) = (1 lsl c) land state_index > 0 in
  List.(fold_left (&&) true (map check controls))

let lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.split_on_char '\n'
