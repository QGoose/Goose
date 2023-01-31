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
