(lang dune 3.6)

(generate_opam_files true)

(name sampling)

(version 1.0.0)

(maintainers "Ryan M. Moore")

(authors "Ryan M. Moore")

(homepage "https://github.com/mooreryan/sampling")

(source
 (github mooreryan/InteinFinder))

(documentation "https://github.com/mooreryan/sampling")

(bug_reports "https://github.com/mooreryan/sampling/issues")

(package
 (name sampling)
 (synopsis "Sampling seqs and such")
 (depends
  (ocaml
   (>= "4.14.0"))
  (bio_io
   (and
    (>= "0.6.0")
    (< "0.7.0")))
  (cmdliner
   (and
    (>= "1.1.0")
    (< "1.2.0")))
  (core
   (and
    (>= "v0.15.0")
    (< "v0.16.0")))
  (core_unix
   (and
    (>= "v0.15.0")
    (< "v0.16.0")))
  (logs
   (and
    (>= "0.7.0")
    (< "0.8.0")))
  (fmt
   (and
    (>= "0.9.0")
    (< "0.10.0")))
  (ocamlformat
   (= "0.23.0"))
  (ppx_jane
   (and
    (>= "v0.15.0")
    (< "v0.16.0")))
  (shexp
   (and
    (>= "v0.15.0")
    (< "v0.16.0")))))
