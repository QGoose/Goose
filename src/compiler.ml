open Utils

let rec resolve_qaddresses (f : Qasm.stmt list) (count : int) =
  match f with
  | [] -> (fun n _ -> failwith @@ Printf.sprintf "qreg '%s' does not exist" n)
  | Qasm.Qreg (Qasm.Id name, Qasm.Nnint sz) :: tail ->
    (fun (name' : string) (idx : int) ->
       if name = name'
       then if idx < sz
            then count + idx
            else failwith @@ Printf.sprintf "index %d is out of bounds, qreg '%s' was previously declared with size %d" idx name sz
       else (resolve_qaddresses tail (sz + count)) name' idx)
  | _ :: tail -> resolve_qaddresses tail count

let compile (_prog : Qasm.t) : Circuit.t =
  let address_for = resolve_qaddresses _prog.body 0 in
  (* e.g. *) let _ = address_for "a" 2 in
  todo ()
