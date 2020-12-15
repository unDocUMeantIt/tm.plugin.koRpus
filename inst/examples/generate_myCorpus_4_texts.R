# use readCorpus() to create an object of class kRp.corpus
# code is only run when the english language package can be loaded
if(require("koRpus.lang.en", quietly = TRUE)){
  myCorpus <- readCorpus(
    dir=file.path(path.package("tm.plugin.koRpus"), "examples", "corpus"),
    hierarchy=list(
      Topic=c(
        Winner="Reality Winner",
        Edwards="Natalie Edwards"
      ),
      Source=c(
        Wikipedia_prev="Wikipedia (old)",
        Wikipedia_new="Wikipedia (new)"
      )
    ),
    # use tokenize() so examples run without a TreeTagger installation
    tagger="tokenize",
    lang="en"
  )
} else {}

