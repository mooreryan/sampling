open! Core
open Lib

module Version = struct
  let base = "1.0.0"

  let version =
    (* git describe --always --dirty --abbrev=7 *)
    let git_hash =
      match%const [%getenv "SAMPLING_GIT_COMMIT_HASH"] with
      | "" ->
          ""
      | git_hash ->
          [%string " [%{git_hash}]"]
    in
    [%string "%{base}%{git_hash}"]
end

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

  let info =
    let doc = "sample sequences from a FASTA file" in
    let man =
      [ `S Manpage.s_description
      ; `P
          "Samples sequences from a FASTA file by 1) determining the number of \
           sequences in the file with ripgrep, 2) generating random sequence \
           indices across all output files (i.e., no repeats across ALL \
           sampled sequences) , and 3) iterating through the file to do the \
           sampling."
      ; `P
          "If the sample_size * num_samples is greater than the total number \
           of sequences per sample, then some of the files will have fewer \
           sequences than expected given the input arguments.  In this case, \
           it will basically shuffle sequences into the given number of files."
      ]
    in
    Cmd.info prog_name ~version:Version.version ~doc ~man ~exits:[]

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

let get_indices ~total_seqs ~seqs_per_sample ~num_samples =
  Iter.(0 -- (total_seqs - 1))
  |> Iter.sample (seqs_per_sample * num_samples)
  |> Set.of_array (module Int)

let write_samples in_file out_channels indices =
  let num_samples = Array.length out_channels in
  let log i =
    if i % 500000 = 0 then
      let i = Float.(of_int i / of_int 1000000) in
      eprintf "Reading seq: %.1fM\r%!" i
  in
  let open Bio_io.Fasta in
  In_channel.with_file_foldi_records
    in_file
    ~init:0
    ~f:(fun i oc_index record ->
      log i ;
      if Set.mem indices i then
        let oc = Array.get out_channels (oc_index % num_samples) in
        let _ = Out_channel.output_string oc (Record.to_string_nl record) in
        oc_index + 1
      else oc_index )
  |> ignore

let close_out_channels out_channels =
  Array.iter out_channels ~f:Out_channel.close

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
  Logs.info (fun m -> m "Generating random indices") ;
  let indices =
    get_indices
      ~num_samples:opts.nsamples
      ~seqs_per_sample:opts.nseqs
      ~total_seqs
  in
  Logs.info (fun m -> m "Sampling...") ;
  write_samples opts.file out_channels indices ;
  close_out_channels out_channels ;
  Logs.info (fun m -> m "Done!")

let () = main ()
