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

context("class kRp.hierarchy")

test_that("creating a 'flat' kRp.hierarchy class object", {
  sampleTextDir <- normalizePath(file.path("samples","C3S","Wikipedia_alt"))
  load("mySimpleCorpus.RData")

  mySimpleCorpus.test <- readCorpus(
    dir=sampleTextDir,
    lang="xy",
    tagger="tokenize"
  )

  # we have to manually set the paths because they would reference the
  # test environment which would cause a string mismatch
  corpusPath(mySimpleCorpus.test) <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  meta(corpusTm(mySimpleCorpus.test))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  # manually set the timetamps of the tm objects, these can't be equal
  meta(corpusTm(mySimpleCorpus.test)[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")

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

test_that("creating a kRp.hierarchy class object", {
  sampleTextDir <- normalizePath(file.path("samples","C3S"))
  load("mySourcesCorpus.RData")

  mySourcesCorpus.test <- readCorpus(
    dir=sampleTextDir,
    hierarchy=list(
      Source=c(
        Wikipedia_alt="Wikipedia (alt)",
        Wikipedia_neu="Wikipedia (neu)"
      )
    ),
    lang="xy",
    tagger="tokenize"
  )

  # we have to manually set the paths because they would reference the
  # test environment which would cause a string mismatch
  corpusPath(mySourcesCorpus.test) <- "tm.plugin.koRpus/tests/testthat/samples/C3S"
  slot(corpusChildren(mySourcesCorpus.test)[["Wikipedia (alt)"]], "path") <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  slot(corpusChildren(mySourcesCorpus.test)[["Wikipedia (neu)"]], "path") <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu"
  meta(corpusTm(corpusChildren(mySourcesCorpus.test)[["Wikipedia (alt)"]]))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  meta(corpusTm(corpusChildren(mySourcesCorpus.test)[["Wikipedia (neu)"]]))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu"
  # the same for the timetamps of the tm objects
  meta(corpusTm(corpusChildren(mySourcesCorpus.test)[["Wikipedia (alt)"]])[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")
  meta(corpusTm(corpusChildren(mySourcesCorpus.test)[["Wikipedia (neu)"]])[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")

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

test_that("creating a kRp.hierarchy class object with two levels", {
  sampleTextDir <- normalizePath(file.path("samples"))
  load("myTopicCorpus.RData")

  myTopicCorpus.test <- readCorpus(
    dir=sampleTextDir,
    hierarchy=list(
      Topic=c(
        C3S="C3S",
        GEMA="GEMA"
      ),
      Source=c(
        Wikipedia_alt="Wikipedia (alt)",
        Wikipedia_neu="Wikipedia (neu)"
      )
    ),
    lang="xy",
    tagger="tokenize"
  )

  # we have to manually set the paths because they would reference the
  # test environment which would cause a string mismatch
  corpusPath(myTopicCorpus.test) <- "tm.plugin.koRpus/tests/testthat/samples"
  corpusPath(corpusChildren(myTopicCorpus.test)[["C3S"]]) <- "tm.plugin.koRpus/tests/testthat/samples/C3S"
  corpusPath(corpusChildren(myTopicCorpus.test)[["GEMA"]]) <- "tm.plugin.koRpus/tests/testthat/samples/GEMA"
  slot(corpusChildren(corpusChildren(myTopicCorpus.test)[["C3S"]])[["Wikipedia (alt)"]], "path") <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  slot(corpusChildren(corpusChildren(myTopicCorpus.test)[["C3S"]])[["Wikipedia (neu)"]], "path") <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu"
  slot(corpusChildren(corpusChildren(myTopicCorpus.test)[["GEMA"]])[["Wikipedia (alt)"]], "path") <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_alt"
  slot(corpusChildren(corpusChildren(myTopicCorpus.test)[["GEMA"]])[["Wikipedia (neu)"]], "path") <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_neu"
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus.test)[["C3S"]])[["Wikipedia (alt)"]]))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus.test)[["C3S"]])[["Wikipedia (neu)"]]))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu"
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus.test)[["GEMA"]])[["Wikipedia (alt)"]]))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_alt"
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus.test)[["GEMA"]])[["Wikipedia (neu)"]]))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_neu"

  # the same for the timetamps of the tm objects
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus.test)[["C3S"]])[["Wikipedia (alt)"]])[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus.test)[["C3S"]])[["Wikipedia (neu)"]])[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus.test)[["GEMA"]])[["Wikipedia (alt)"]])[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus.test)[["GEMA"]])[["Wikipedia (neu)"]])[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")

  expect_equal(
    myTopicCorpus,
    myTopicCorpus.test
  )
})


context("export to TIF")

test_that("exporting a kRp.hierarchy class object to TIF data frame", {
  load("myTopicCorpus.RData")
  load("myTopicCorpus_TIF_df.RData")
  
  myTopicCorpus_TIF_df.test <- tif_as_corpus_df(myTopicCorpus)

  expect_equal(
    myTopicCorpus_TIF_df,
    myTopicCorpus_TIF_df.test
  )
})


context("import from TIF")

test_that("importing a TIF data frame as a kRp.hierarchy class object", {
  load("myTopicCorpus_TIF_df.RData")
  load("myTopicCorpus_from_df.RData")
  
  myTopicCorpus_from_df.test <- readCorpus(
    dir=myTopicCorpus_TIF_df,
    hierarchy=list(
      Topic=c(
        C3S="C3S",
        GEMA="GEMA"
      ),
      Source=c(
        Wikipedia_alt="Wikipedia (alt)",
        Wikipedia_neu="Wikipedia (neu)"
      )
    ),
    lang="xy",
    tagger="tokenize",
    format="obj"
  )
  
  # same correctzions as with myTopicCorpus.test above
  corpusPath(myTopicCorpus_from_df.test) <- "tm.plugin.koRpus/tests/testthat/samples"
  corpusPath(corpusChildren(myTopicCorpus_from_df.test)[["C3S"]]) <- "tm.plugin.koRpus/tests/testthat/samples/C3S"
  corpusPath(corpusChildren(myTopicCorpus_from_df.test)[["GEMA"]]) <- "tm.plugin.koRpus/tests/testthat/samples/GEMA"
  slot(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["C3S"]])[["Wikipedia (alt)"]], "path") <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  slot(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["C3S"]])[["Wikipedia (neu)"]], "path") <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu"
  slot(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["GEMA"]])[["Wikipedia (alt)"]], "path") <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_alt"
  slot(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["GEMA"]])[["Wikipedia (neu)"]], "path") <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_neu"
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["C3S"]])[["Wikipedia (alt)"]]))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["C3S"]])[["Wikipedia (neu)"]]))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu"
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["GEMA"]])[["Wikipedia (alt)"]]))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_alt"
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["GEMA"]])[["Wikipedia (neu)"]]))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_neu"
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["C3S"]])[["Wikipedia (alt)"]])[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["C3S"]])[["Wikipedia (neu)"]])[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["GEMA"]])[["Wikipedia (alt)"]])[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")
  meta(corpusTm(corpusChildren(corpusChildren(myTopicCorpus_from_df.test)[["GEMA"]])[["Wikipedia (neu)"]])[[1]])$datetimestamp <- as.POSIXlt("2018-07-29 01:01:01", tz="GMT")
  
  expect_equal(
    myTopicCorpus_from_df,
    myTopicCorpus_from_df.test
  )
})

# # new test standards
# myTopicCorpus <- myTopicCorpus.test
# save(myTopicCorpus,
#   file=file.path(main.root,"tests","testthat","myTopicCorpus.RData"),
#   compress="xz",
#   compression_level=-9)
