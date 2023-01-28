let todo () = failwith "Unimplemented"

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
