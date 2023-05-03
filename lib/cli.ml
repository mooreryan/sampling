open! Core
open Cmdliner

let ( let+ ) v f = Term.(const f $ v)

let ( and+ ) v1 v2 = Term.(const (fun x y -> (x, y)) $ v1 $ v2)

let non_existing_file =
  let parser =
    Arg.parser_of_kind_of_string
      ~kind:"a file/dir that does not exist"
      (fun s -> if Sys_unix.file_exists_exn s then None else Some s)
  in
  let printer = Format.pp_print_string in
  Arg.conv (parser, printer)

let strictly_positive_int =
  let parser =
    Arg.parser_of_kind_of_string ~kind:"an int >= 1" (fun s ->
        let open Option.Let_syntax in
        let%bind n = try Some (Int.of_string s) with _ -> None in
        if n >= 1 then Some n else None )
  in
  let printer = Format.pp_print_int in
  Arg.conv (parser, printer)

(* Shared code between the sampling programs *)
module Sampling_shared = struct
  type opts =
    { file: string
    ; outdir: string
    ; basename: string
    ; nseqs: int
    ; nsamples: int
    ; seed: int }
  [@@deriving sexp_of]

  let opts_to_string opts = Sexp.to_string @@ [%sexp_of: opts] opts

  let file =
    let doc = "Path to fasta file (should exist)" in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FASTA" ~doc)

  let outdir =
    let doc = "Path to output directory (should not exist)" in
    Arg.(
      value
      & opt non_existing_file "samples"
      & info ["outdir"] ~docv:"OUTDIR" ~doc )

  let basename =
    let doc = "Basename for output files" in
    Arg.(value & opt string "seqs" & info ["basename"] ~docv:"BASENAME" ~doc)

  let nseqs =
    let doc = "Size of sample (i.e., number of seqs per sample)" in
    Arg.(
      required
      & pos 1 (some strictly_positive_int) None
      & info [] ~docv:"SEQS_PER_SAMPLE" ~doc )

  let nsamples =
    let doc = "Number of samples to take" in
    Arg.(
      value
      & opt strictly_positive_int 1
      & info ["samples"] ~docv:"NUM_SAMPLES" ~doc )

  let seed =
    let doc = "Seed for random number generator" in
    Arg.(
      value
      & opt strictly_positive_int 30355
      & info ["seed"] ~docv:"NUM_SAMPLES" ~doc )

  let opts : opts Term.t =
    let+ file and+ outdir and+ basename and+ nseqs and+ nsamples and+ seed in
    {file; outdir; basename; nseqs; nsamples; seed}
end
