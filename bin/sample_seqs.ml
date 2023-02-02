open! Core
open Lib

module Cli = struct
  open Cmdliner

  let ( let+ ) v f = Term.(const f $ v)

  let ( and+ ) v1 v2 = Term.(const (fun x y -> (x, y)) $ v1 $ v2)

  let prog_name = "sample_seqs"

  type opts =
    { file: string
    ; outdir: string
    ; basename: string
    ; nseqs: int
    ; nsamples: int
    ; seed: int }
  [@@deriving sexp_of]

  let opts_to_string opts = Sexp.to_string @@ [%sexp_of: opts] opts

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
      & info [] ~docv:"NUM_SEQS" ~doc )

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

  let info =
    let doc = "sample sequences from a FASTA file" in
    let man =
      [ `S Manpage.s_description
      ; `P
          "Samples sequences from a FASTA file by 1) determining the number of \
           sequences in the file with ripgrep, 2) generating random sequence \
           indices for each file (w/o replacement within a single output file) \
           to sample, and 3) iterating through the file to do the sampling." ]
    in
    let version = "1.0.0" in
    Cmd.info prog_name ~version ~doc ~man ~exits:[]

  let parse_argv () =
    match Cmd.eval_value @@ Cmd.v info opts with
    | Ok (`Ok opts) ->
        opts
    | Ok `Help | Ok `Version ->
        exit 0
    | Error _ ->
        exit 1
end

let total_seqs file =
  Shexp_process.run "rg" ["-c"; "^>"; file]
  |> Shexp_process.capture [Shexp_process.Std_io.Stdout]
  |> Shexp_process.eval
  |> snd
  |> String.strip
  |> Int.of_string

let out_channels ~outdir ~basename ~nsamples =
  let out_channel i =
    let name =
      Filename_unix.realpath outdir ^/ [%string "%{basename}.%{i#Int}.fa"]
    in
    Out_channel.create ~fail_if_exists:true name
  in
  Array.init nsamples ~f:out_channel

let random_samples ~num_samples ~seqs_per_sample ~total_seqs =
  let random_sample ~num_to_take ~total_seqs =
    let rec add_n numbers =
      let n = Random.int total_seqs in
      if Set.mem numbers n then add_n numbers else Set.add numbers n
    in
    let rec loop numbers =
      if Set.length numbers >= num_to_take then numbers
      else loop (add_n numbers)
    in
    loop Int.Set.empty
  in
  Array.init num_samples ~f:(fun _ ->
      random_sample ~num_to_take:seqs_per_sample ~total_seqs )

module Sample = struct
  type t = {out_channel: Out_channel.t; indices: Int.Set.t}

  let create out_channel indices = {out_channel; indices}
end

let write_samples file samples =
  let open Bio_io.Fasta in
  In_channel.with_file_iteri_records file ~f:(fun i record ->
      Array.iter samples ~f:(fun Sample.{out_channel; indices} ->
          if Set.mem indices i then
            Out_channel.output_string out_channel (Record.to_string_nl record) ) )

let main () =
  Logging.set_up_logging "debug" ;
  let opts = Cli.parse_argv () in
  Logs.info (fun m -> m "%s" @@ Cli.opts_to_string opts) ;
  Core_unix.mkdir_p opts.outdir ;
  Random.init opts.seed ;
  Logs.info (fun m -> m "Making out channels") ;
  let out_channels =
    out_channels
      ~outdir:opts.outdir
      ~basename:opts.basename
      ~nsamples:opts.nsamples
  in
  Logs.info (fun m -> m "Counting total number seqs") ;
  let total_seqs = total_seqs opts.file in
  Logs.info (fun m -> m "Total number of seqs: %d" total_seqs) ;
  Logs.info (fun m -> m "Getting random samples") ;
  let random_samples =
    random_samples
      ~num_samples:opts.nsamples
      ~seqs_per_sample:opts.nseqs
      ~total_seqs
  in
  let samples = Array.map2_exn out_channels random_samples ~f:Sample.create in
  Logs.info (fun m -> m "Sampling...") ;
  write_samples opts.file samples ;
  Logs.info (fun m -> m "Done!")

let () = main ()
