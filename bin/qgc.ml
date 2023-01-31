open Quantumlib

let usage_msg = "goose [-verbose] <file1> [<file2>] ... -o <output>"

type input_format = OpenQasm20 of string | GooseIR of string

type command = ToIR of string | ToC of string | Simulate | Symex

type args_builder = {
  mutable src_format : input_format option;
  mutable command : command option;
}

let my_args = { src_format = None; command = None }

let abort () =
  Printf.eprintf "usage: %s\n" usage_msg;
  exit 1

let with_input (inp : string) : unit =
  try begin
    match Filename.extension inp with
    | ".qasm" -> my_args.src_format <- Some (OpenQasm20 inp)
    | ".ir" -> my_args.src_format <- Some (GooseIR inp)
    | ext ->
      Printf.eprintf "unsupported input file extension %s\n" ext;
      abort ()
  end with _ ->
    Printf.eprintf "invalid input file name %s\n" inp;
    abort ()

let with_output (out : string) : unit =
  try begin
    match Filename.extension out with
    | ".c" -> my_args.command <- Some (ToC out)
    | ".ir" -> my_args.command <- Some (ToIR out)
    | ext ->
      Printf.eprintf "unsupported output file extension %s\n" ext;
      abort ()
  end with _ ->
    Printf.eprintf "invalid output file name %s\n" out;
    abort ()

let with_command (cmd : command) () : unit =
  my_args.command <- Some cmd

let speclist =
  [
    ("-i", Arg.String with_input, "Name of the input file (*.ir or *.qasm)");
    ("-o", Arg.String with_output, "Compile source to GooseIR");
    ("-run", Arg.Unit (with_command Simulate), "Simulate the input circuit");
    ("-symex", Arg.Unit (with_command Symex), "(Symbolically) Simulate the input");
  ]

let execute () =
  match my_args.src_format, my_args.command with
  | Some (OpenQasm20 inp), Some (ToC out) ->
    let outc = open_out out in
    Parser.parse_file inp
    |> Compiler.compile
    |> C_emitter.emitc outc;
    close_out outc
  | Some (OpenQasm20 _), Some (ToIR _) ->
    Printf.eprintf "Conversion from OpenQasm to IR not implemented\n";
    exit 1
  | Some (GooseIR _), Some _ ->
    Printf.eprintf "Processing of IR not implemented\n";
    exit 1
  | Some (OpenQasm20 inp), Some Simulate ->
    Parser.parse_file inp
    |> Compiler.compile
    |> Naive.NaiveSimulator.run
    |> Naive.NaiveBackend.print_state stdout
  | Some (OpenQasm20 inp), Some Symex ->
    Parser.parse_file inp
    |> Compiler.compile
    |> Se.SE_Engine.run
    |> Se.SBackend.print_state stdout
  | None, _ ->
    Printf.eprintf "No input provided\n";
    abort ()
  | _, None ->
    Printf.eprintf "No output provided\n";
    abort ()

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  execute ()


(* type source_file = SrcFile of string *)

(* type input_format = OpenQasm20 | GooseIR *)

(* let str_to_input_format (s : string) =
  match s with
  | "openqasm2.0" -> OpenQasm20
  | "goose_ir" -> GooseIR
  | _ -> failwith "unknown input format"

let compiler (in_fmt : input_format) : source_file -> Circuit.t =
  match in_fmt with
  | OpenQasm20 ->
    fun src -> let SrcFile src = src in
      let qasm_ast = Parser.parse_file src in
      Compiler.compile qasm_ast
  | GooseIR -> failwith "Goose IR frontend not implemented"

type output_format = GooseIR | NaiveSim | CSeSim

let str_to_output_format (s: string) = match s with
  | "goose_ir" -> GooseIR
  | "naive_sim" -> NaiveSim
  | "c_se_sim" -> CSeSim
  | _ -> failwith "unknown output format"

let emitter (out_fmt : output_format) : Circuit.t -> string =
  match out_fmt with
  | GooseIR -> Circuit.string_of_circuit
  | NaiveSim -> Utils.todo ()
  | CSeSim -> Utils.todo ()

type args_builder = {
  in_fmt : input_format option;
  out_fmt : output_format option;
  src_file : source_file option;
}

let with_in_fmt (args : args_builder) (in_fmt : input_format) : args_builder =
  match args.in_fmt with
  | None -> {
      in_fmt = Some in_fmt;
      out_fmt = args.out_fmt;
      src_file = args.src_file;
    }
  | Some _ -> failwith "already have an input format"

let with_out_fmt (args : args_builder) (out_fmt : output_format) : args_builder =
  match args.out_fmt with
  | None -> {
      in_fmt = args.in_fmt;
      out_fmt = Some out_fmt;
      src_file = args.src_file;
    }
  | Some _ -> failwith "already have an output format"

let with_src_file (args : args_builder) (src_file : source_file) : args_builder =
  match args.src_file with
  | None -> {
      in_fmt = args.in_fmt;
      out_fmt = args.out_fmt;
      src_file = Some src_file;
    }
  | Some SrcFile orig ->
    let SrcFile src_file = src_file in
    failwith (Printf.sprintf "Tried to use source file `%s`, but already have source file `%s`." src_file orig)

type arguments = {
  in_fmt : input_format;
  out_fmt : output_format;
  src_file : source_file;
}

let builder_to_arguments (args : args_builder) : arguments =
  let in_fmt = match args.in_fmt with
    | Some x -> x
    | None -> failwith "no input format parsed" in
  let out_fmt = match args.out_fmt with
    | Some x -> x
    | None -> failwith "no output format parsed" in
  let src_file = match args.src_file with
    | Some x -> x
    | None -> failwith "no source file parsed" in
  {
    in_fmt = in_fmt;
    out_fmt = out_fmt;
    src_file = src_file;
  }

(* Hand-roll a state machine to parse the args_builder *)
type expecting_kind = Any | InFmt | OutFmt

let parse_arg (state : expecting_kind * args_builder) (arg : string) : expecting_kind * args_builder =
  let (expecting, args) = state in
  match expecting with
  | Any -> (
      match arg with
      | "-in" -> (InFmt, args)
      | "-out" -> (OutFmt, args)
      (* If not an `-in` or `-out`, arg is a source file *)
      | _ -> (Any, with_src_file args (SrcFile arg))
    )
  | InFmt ->
    let in_fmt = str_to_input_format arg in
    let new_args = with_in_fmt args in_fmt in
    (Any, new_args)
  | OutFmt ->
    let out_fmt = str_to_output_format arg in
    let new_args = with_out_fmt args out_fmt in
    (Any, new_args)

let parse_args : arguments =
  (* Remove the first element, which is just this binary *)
  let cli_args = Array.to_seq Sys.argv |> Seq.drop 1 in
  let (empty_args : args_builder) = { in_fmt = None; out_fmt = None; src_file = None; } in
  let (_, final_args) = Seq.fold_left parse_arg (Any, empty_args) cli_args in
  builder_to_arguments final_args

let () =
  let args = parse_args in
  args.src_file |>
  compiler args.in_fmt |>
  emitter args.out_fmt |>
  print_endline *)
