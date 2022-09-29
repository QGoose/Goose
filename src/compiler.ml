open Utils


let rec collect_addresses_inner (insts : Qasm.stmt list) (address_count : int) =
  match insts with
  | [] -> (fun _ _ -> failwith "register does not exist")
  | Qasm.Qreg (Qasm.Id name, Qasm.Nnint sz) :: tail ->
    (fun (name' : string) (idx : int) ->
       let rest = collect_addresses_inner tail (address_count + sz) in
       if
         name == name'
       then
         address_count + idx
       else
         rest name' idx)
  | _ :: tail -> collect_addresses_inner tail address_count

let collect_addresses (prog : Qasm.t) =
  let insts = prog.body in
  collect_addresses_inner insts 0

let compile (prog : Qasm.t) : Circuit.t =
  let address_map = collect_addresses prog in
  let _ = Printf.printf "%i" (address_map "a" 0) in
  todo ()
