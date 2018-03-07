# add support for pseudo language xyzedish to be independent from any actual language support in the package
set.lang.support("kRp.POS.tags",
  ## tag and class definitions
  # xy -- xyzedish
  # see http://www.ims.uni-stuttgart.de/projekte/corplex/TreeTagger/Penn-Treebank-Tagset.pdf
  list("xy"=list(
    tag.class.def.words=matrix(c(
      "CC", "conjunction", "Coordinating conjunction"
      ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
    tag.class.def.punct=matrix(c(
      ",", "comma", "Comma"
      ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
    tag.class.def.sentc=matrix(c(
      "SENT", "fullstop", "Sentence ending punctuation" # not in guidelines
      ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
    )
  )
)

context("class kRp.corpus")

test_that("creating a kRp.corpus class object", {
  sampleTextDir <- normalizePath(file.path("samples","C3S","Wikipedia_alt"))
  load("mySimpleCorpus.RData")

  mySimpleCorpus.test <- simpleCorpus(dir=sampleTextDir, lang="xy", tagger="tokenize")
  # manually set the timetamps of the tm objects, these can't be equal
  meta(slot(mySimpleCorpus.test, "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2018-03-07 01:01:01", tz="GMT")

  expect_equal(
    mySimpleCorpus.test,
    mySimpleCorpus
  )
})
# # new test standards
# main.root <- file.path("path/to/tm.plugin.koRpus")
# mySimpleCorpus <- mySimpleCorpus.test
# save(mySimpleCorpus,
#   file=file.path(main.root,"tests","testthat","mySimpleCorpus.RData"),
#   compress="xz",
#   compression_level=-9)

test_that("creating a kRp.sourcesCorpus class object", {
  sampleTextDir <- normalizePath(file.path("samples","C3S"))
  load("mySourcesCorpus.RData")

  sampleSources <- c(
    wpa="Wikipedia_alt",
    wpn="Wikipedia_neu"
  )
  mySourcesCorpus.test <- sourcesCorpus(
    path=sampleTextDir,
    sources=sampleSources,
    topic="C3S",
    tagger="tokenize",
    lang="xy"
  )
    
  # we have to manually set the paths because they would reference the
  # test environment which would cause a string mismatch
  slot(mySourcesCorpus.test, "paths") <- list(
    wpa="tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt/C3S_2013-09-24.txt",
    wpn="tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu/C3S_2015-07-05.txt"
  )
  # the same for the timetamps of the tm objects
  meta(
    slot(
      slot(mySourcesCorpus.test, "sources")[["wpn"]],
      "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2018-03-07 01:01:01", tz="GMT")
  meta(
    slot(
      slot(mySourcesCorpus.test, "sources")[["wpa"]],
      "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2018-03-07 01:01:01", tz="GMT")

  expect_equal(
    mySourcesCorpus,
    mySourcesCorpus.test
  )
})
# # new test standards
# mySourcesCorpus <- mySourcesCorpus.test
# save(mySourcesCorpus,
#   file=file.path(main.root,"tests","testthat","mySourcesCorpus.RData"),
#   compress="xz",
#   compression_level=-9)

test_that("creating a kRp.topicCorpus class object", {
  sampleTextDirC3S <- normalizePath(file.path("samples","C3S"))
  sampleTextDirGEMA <- normalizePath(file.path("samples","GEMA"))
  load("myTopicCorpus.RData")

  samplePaths <- c(
    C3S=sampleTextDirC3S,
    GEMA=sampleTextDirGEMA
  )
  sampleSources <- c(
    wpa="Wikipedia_alt",
    wpn="Wikipedia_neu"
  )
  myTopicCorpus.test <- topicCorpus(
    paths=samplePaths,
    sources=sampleSources,
    tagger="tokenize",
    lang="xy"
  )

  # we have to manually set the paths because they would reference the
  # test environment which would cause a string mismatch
  slot(slot(myTopicCorpus.test, "topics")[["C3S"]], "paths") <- list(
    wpa="tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt/C3S_2013-09-24.txt",
    wpn="tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu/C3S_2015-07-05.txt"
  )
  slot(slot(myTopicCorpus.test, "topics")[["GEMA"]], "paths") <- list(
    wpa="tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_alt/GEMA_2013-09-26.txt",
    wpn="tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_neu/GEMA_2015-07-05.txt"
  )
  # the same for the timetamps of the tm objects
  meta(
    slot(
      slot(
        slot(myTopicCorpus.test, "topics")[["C3S"]], "sources")[["wpa"]],
        "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2018-03-07 01:01:01", tz="GMT")
  meta(
    slot(
      slot(
        slot(myTopicCorpus.test, "topics")[["C3S"]], "sources")[["wpn"]],
        "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2018-03-07 01:01:01", tz="GMT")
  meta(
    slot(
      slot(
        slot(myTopicCorpus.test, "topics")[["GEMA"]], "sources")[["wpa"]],
        "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2018-03-07 01:01:01", tz="GMT")
  meta(
    slot(
      slot(
        slot(myTopicCorpus.test, "topics")[["GEMA"]], "sources")[["wpn"]],
        "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2018-03-07 01:01:01", tz="GMT")

  expect_equal(
    myTopicCorpus,
    myTopicCorpus.test
  )
})
# # new test standards
# myTopicCorpus <- myTopicCorpus.test
# save(myTopicCorpus,
#   file=file.path(main.root,"tests","testthat","myTopicCorpus.RData"),
#   compress="xz",
#   compression_level=-9)
