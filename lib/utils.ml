open! Core

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

let log i =
  if i % 500000 = 0 then
    let i = Float.(of_int i / of_int 1000000) in
    eprintf "Reading seq: %.1fM\r%!" i
