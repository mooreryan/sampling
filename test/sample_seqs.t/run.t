Yo

  $ sample_seqs seqs.fa 3 --samples 4 2> log
  $ cut -f4- -d' ' log
  ((file seqs.fa)(outdir samples)(basename seqs)(nseqs 3)(nsamples 4)(seed 30355))
  Making out channels
  Counting total number seqs
  Total number of seqs: 10
  Getting random samples
  Sampling...
  Done!
  $ tree
  .
  |-- log
  |-- samples
  |   |-- seqs.0.fa
  |   |-- seqs.1.fa
  |   |-- seqs.2.fa
  |   `-- seqs.3.fa
  `-- seqs.fa -> ../../../../../default/test/sample_seqs.t/seqs.fa
  
  1 directory, 6 files
  $ head -n1000 samples/*
  ==> samples/seqs.0.fa <==
  >s08
  h
  >s09
  i
  >s10
  j
  
  ==> samples/seqs.1.fa <==
  >s03
  c
  >s07
  g
  >s08
  h
  
  ==> samples/seqs.2.fa <==
  >s01
  a
  >s02
  b
  >s10
  j
  
  ==> samples/seqs.3.fa <==
  >s02
  b
  >s07
  g
  >s08
  h
