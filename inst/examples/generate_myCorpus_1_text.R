  myCorpus <- readCorpus(
    dir=file.path(
      path.package("tm.plugin.koRpus"), "examples", "corpus", "Winner", "Wikipedia_new"
    ),
    # use tokenize() so examples run without a TreeTagger installation
    tagger="tokenize",
    lang="en"
  )
