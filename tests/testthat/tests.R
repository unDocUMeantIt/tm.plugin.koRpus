context("class kRp.corpus")

test_that("creating a kRp.corpus class object", {
  sampleTextDir <- normalizePath(file.path("samples","C3S","Wikipedia_alt"))
  load("mySimpleCorpus.RData")

  mySimpleCorpus.test <- simpleCorpus(dir=sampleTextDir, lang="de", tagger="tokenize")
  # manually set the timetamps of the tm objects, these can't be equal
  meta(slot(mySimpleCorpus.test, "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2015-07-05 16:00:00", tz="GMT")

  expect_that(
    mySimpleCorpus.test,
    equals(mySimpleCorpus)
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
  mySourcesCorpus.test <- sourcesCorpus(path=sampleTextDir, sources=sampleSources, topic="C3S",
    tagger="tokenize", lang="de"
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
      "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2015-07-05 16:00:00", tz="GMT")
  meta(
    slot(
      slot(mySourcesCorpus.test, "sources")[["wpa"]],
      "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2015-07-05 16:00:00", tz="GMT")
  
  expect_that(
    mySourcesCorpus,
    equals(mySourcesCorpus.test)
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
  myTopicCorpus.test <- topicCorpus(paths=samplePaths, sources=sampleSources,
    tagger="tokenize", lang="de"
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
        "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2015-07-05 16:00:00", tz="GMT")
  meta(
    slot(
      slot(
        slot(myTopicCorpus.test, "topics")[["C3S"]], "sources")[["wpn"]],
        "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2015-07-05 16:00:00", tz="GMT")
  meta(
    slot(
      slot(
        slot(myTopicCorpus.test, "topics")[["GEMA"]], "sources")[["wpa"]],
        "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2015-07-05 16:00:00", tz="GMT")
  meta(
    slot(
      slot(
        slot(myTopicCorpus.test, "topics")[["GEMA"]], "sources")[["wpn"]],
        "raw")[["tm"]][[1]])$datetimestamp <- as.POSIXlt("2015-07-05 16:00:00", tz="GMT")

  expect_that(
    myTopicCorpus,
    equals(myTopicCorpus.test)
  )
})
# # new test standards
# myTopicCorpus <- myTopicCorpus.test
# save(myTopicCorpus,
#   file=file.path(main.root,"tests","testthat","myTopicCorpus.RData"),
#   compress="xz",
#   compression_level=-9)
