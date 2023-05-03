open! Core

(* git describe --always --dirty --abbrev=7 *)
let git_hash =
  match%const [%getenv "SAMPLING_GIT_COMMIT_HASH"] with
  | "" ->
      ""
  | git_hash ->
      [%string " [%{git_hash}]"]
