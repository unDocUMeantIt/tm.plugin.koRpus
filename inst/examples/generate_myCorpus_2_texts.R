  myCorpus <- readCorpus(
    dir=file.path(
      path.package("tm.plugin.koRpus"), "examples", "corpus", "Edwards"
    ),
    hierarchy=list(
      Source=c(
        Wikipedia_prev="Wikipedia (old)",
        Wikipedia_new="Wikipedia (new)"
      )
    ),
    # use tokenize() so examples run without a TreeTagger installation
    tagger="tokenize",
    lang="en"
  )
