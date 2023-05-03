open! Core
open Lib

module Version = struct
  let base = "1.0.0"

  let version = [%string "%{base}%{Lib.Version.git_hash}"]
end

module Cli = struct
  open Cmdliner
  include Lib.Cli
  include Lib.Cli.Sampling_shared

  let prog_name = "sample_seqs"

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

let get_indices ~total_seqs ~seqs_per_sample ~num_samples =
  Iter.(0 -- (total_seqs - 1))
  |> Iter.sample (seqs_per_sample * num_samples)
  |> Set.of_array (module Int)

let write_samples in_file out_channels indices =
  let num_samples = Array.length out_channels in
  let open Bio_io.Fasta in
  In_channel.with_file_foldi_records
    in_file
    ~init:0
    ~f:(fun i oc_index record ->
      Utils.log i ;
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
    Utils.out_channels
      ~outdir:opts.outdir
      ~basename:opts.basename
      ~nsamples:opts.nsamples
  in
  Logs.info (fun m -> m "Counting total number seqs") ;
  let total_seqs = Utils.total_seqs opts.file in
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
