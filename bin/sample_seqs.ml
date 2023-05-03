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
           indices for each file (w/o replacement within a single output file) \
           to sample, and 3) iterating through the file to do the sampling." ]
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
      Utils.log i ;
      Array.iter samples ~f:(fun Sample.{out_channel; indices} ->
          if Set.mem indices i then
            Out_channel.output_string out_channel (Record.to_string_nl record) ) )

let close_out_channels samples =
  Array.iter samples ~f:(fun Sample.{out_channel; _} ->
      Out_channel.close out_channel )

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
  close_out_channels samples ;
  Logs.info (fun m -> m "Done!")

let () = main ()
