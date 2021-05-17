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

test_that("creating a 'flat' kRp.corpus class object, one single text", {
  sampleTextDir <- normalizePath(file.path("samples","C3S","Wikipedia_alt"))
  load("sampleCorpusHier0Txt1.RData")

  sampleCorpusHier0Txt1.test <- readCorpus(
    dir=sampleTextDir,
    lang="xy",
    tagger="tokenize",
    encoding="UTF-8"
  )

  # we have to manually set the paths because they would reference the
  # test environment which would cause a string mismatch
  meta(corpusTm(sampleCorpusHier0Txt1.test)[[1]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier0Txt1.test))[["path"]] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  # manually set the timetamps of the tm objects, these can't be equal
  meta(corpusTm(sampleCorpusHier0Txt1.test)[[1]])[["datetimestamp"]] <- as.POSIXlt("2020-12-08 01:01:01", tz="GMT")

  expect_equal(
    sampleCorpusHier0Txt1.test,
    sampleCorpusHier0Txt1
  )
})
# # new test standards
# main.root <- file.path(getwd(), "..", "..") # file.path("path/to/tm.plugin.koRpus")
# sampleCorpusHier0Txt1 <- sampleCorpusHier0Txt1.test
# save(sampleCorpusHier0Txt1,
#   file=file.path(main.root,"tests","testthat","sampleCorpusHier0Txt1.RData"),
#   compress="xz",
#   compression_level=-9)

test_that("creating a kRp.corpus class object, one hierarchy level", {
  sampleTextDir <- normalizePath(file.path("samples","C3S"))
  load("sampleCorpusHier1Txt2.RData")

  sampleCorpusHier1Txt2.test <- readCorpus(
    dir=sampleTextDir,
    hierarchy=list(
      Source=c(
        Wikipedia_alt="Wikipedia (alt)",
        Wikipedia_neu="Wikipedia (neu)"
      )
    ),
    lang="xy",
    tagger="tokenize",
    encoding="UTF-8"
  )

  # we have to manually set the paths because they would reference the
  # test environment which would cause a string mismatch
  meta(corpusTm(sampleCorpusHier1Txt2.test)[[1]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier1Txt2.test))[1,"path"] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  meta(corpusTm(sampleCorpusHier1Txt2.test)[[2]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier1Txt2.test))[2,"path"] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu"
  # the same for the timetamps of the tm objects
  meta(corpusTm(sampleCorpusHier1Txt2.test)[[1]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpusHier1Txt2.test)[[2]])[["datetimestamp"]] <- as.POSIXlt("2020-12-08 01:01:01", tz="GMT")

  expect_equal(
    sampleCorpusHier1Txt2,
    sampleCorpusHier1Txt2.test
  )
})
# # new test standards
# sampleCorpusHier1Txt2 <- sampleCorpusHier1Txt2.test
# save(sampleCorpusHier1Txt2,
#   file=file.path(main.root,"tests","testthat","sampleCorpusHier1Txt2.RData"),
#   compress="xz",
#   compression_level=-9)

test_that("creating a kRp.corpus class object, two hierarchy levels", {
  sampleTextDir <- normalizePath(file.path("samples"))
  load("sampleCorpusHier2Txt4.RData")

  sampleCorpusHier2Txt4.test <- readCorpus(
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
    tagger="tokenize",
    encoding="UTF-8"
  )

  # we have to manually set the paths because they would reference the
  # test environment which would cause a string mismatch
  meta(corpusTm(sampleCorpusHier2Txt4.test)[[1]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4.test))[1,"path"] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  meta(corpusTm(sampleCorpusHier2Txt4.test)[[2]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4.test))[2,"path"] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu"
  meta(corpusTm(sampleCorpusHier2Txt4.test)[[3]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4.test))[3,"path"] <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_alt"
  meta(corpusTm(sampleCorpusHier2Txt4.test)[[4]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4.test))[4,"path"] <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_neu"

  # the same for the timetamps of the tm objects
  meta(corpusTm(sampleCorpusHier2Txt4.test)[[1]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4.test)[[2]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4.test)[[3]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4.test)[[4]])[["datetimestamp"]] <- as.POSIXlt("2020-12-08 01:01:01", tz="GMT")

  expect_equal(
    sampleCorpusHier2Txt4,
    sampleCorpusHier2Txt4.test
  )
})
# # new test standards
# sampleCorpusHier2Txt4 <- sampleCorpusHier2Txt4.test
# save(sampleCorpusHier2Txt4,
#   file=file.path(main.root,"tests","testthat","sampleCorpusHier2Txt4.RData"),
#   compress="xz",
#   compression_level=-9)

test_that("creating a kRp.corpus class object, autodetect hierarchy levels", {
  sampleTextDir <- normalizePath(file.path("samples"))
  load("sampleCorpusHier2Txt4_auto.RData")

  sampleCorpusHier2Txt4_auto.test <- readCorpus(
    dir=sampleTextDir,
    hierarchy=TRUE,
    lang="xy",
    tagger="tokenize",
    encoding="UTF-8"
  )

  # we have to manually set the paths because they would reference the
  # test environment which would cause a string mismatch
  meta(corpusTm(sampleCorpusHier2Txt4_auto.test)[[1]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4_auto.test))[1,"path"] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_alt"
  meta(corpusTm(sampleCorpusHier2Txt4_auto.test)[[2]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4_auto.test))[2,"path"] <- "tm.plugin.koRpus/tests/testthat/samples/C3S/Wikipedia_neu"
  meta(corpusTm(sampleCorpusHier2Txt4_auto.test)[[3]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4_auto.test))[3,"path"] <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_alt"
  meta(corpusTm(sampleCorpusHier2Txt4_auto.test)[[4]])[["path"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4_auto.test))[4,"path"] <- "tm.plugin.koRpus/tests/testthat/samples/GEMA/Wikipedia_neu"

  # the same for the timetamps of the tm objects
  meta(corpusTm(sampleCorpusHier2Txt4_auto.test)[[1]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4_auto.test)[[2]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4_auto.test)[[3]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4_auto.test)[[4]])[["datetimestamp"]] <- as.POSIXlt("2020-12-08 01:01:01", tz="GMT")

  expect_equal(
    sampleCorpusHier2Txt4_auto,
    sampleCorpusHier2Txt4_auto.test
  )
})
# # new test standards
# sampleCorpusHier2Txt4_auto <- sampleCorpusHier2Txt4_auto.test
# save(sampleCorpusHier2Txt4_auto,
#   file=file.path(main.root,"tests","testthat","sampleCorpusHier2Txt4_auto.RData"),
#   compress="xz",
#   compression_level=-9)


context("export to TIF")

test_that("exporting a kRp.corpus class object to TIF data frame", {
  load("sampleCorpusHier2Txt4.RData")
  load("sampleCorpusHier2Txt4_TIF_df.RData")

  sampleCorpusHier2Txt4_TIF_df.test <- tif_as_corpus_df(sampleCorpusHier2Txt4)

  expect_equal(
    sampleCorpusHier2Txt4_TIF_df,
    sampleCorpusHier2Txt4_TIF_df.test
  )
})
# # new test standards
# sampleCorpusHier2Txt4_TIF_df <- sampleCorpusHier2Txt4_TIF_df.test
# save(sampleCorpusHier2Txt4_TIF_df,
#   file=file.path(main.root,"tests","testthat","sampleCorpusHier2Txt4_TIF_df.RData"),
#   compress="xz",
#   compression_level=-9)


context("import from TIF")

test_that("importing a TIF data frame as a kRp.corpus class object", {
  load("sampleCorpusHier2Txt4_TIF_df.RData")
  load("sampleCorpusHier2Txt4_from_df.RData")
  
  sampleCorpusHier2Txt4_from_df.test <- readCorpus(
    dir=sampleCorpusHier2Txt4_TIF_df,
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
  
  # path should be preserved
  # the same for the timetamps of the tm objects
  meta(corpusTm(sampleCorpusHier2Txt4_from_df.test)[[1]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4_from_df.test)[[2]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4_from_df.test)[[3]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpusHier2Txt4_from_df.test)[[4]])[["datetimestamp"]] <- as.POSIXlt("2020-12-08 01:01:01", tz="GMT")
  
  expect_equal(
    sampleCorpusHier2Txt4_from_df,
    sampleCorpusHier2Txt4_from_df.test
  )
})
# # new test standards
# sampleCorpusHier2Txt4_from_df <- sampleCorpusHier2Txt4_from_df.test
# save(sampleCorpusHier2Txt4_from_df,
#   file=file.path(main.root,"tests","testthat","sampleCorpusHier2Txt4_from_df.RData"),
#   compress="xz",
#   compression_level=-9)

test_that("importing a minimal TIF data frame as a kRp.corpus class object", {
  load("sampleCorpus_TIF_df_min.RData")
  load("sampleCorpus_from_df_min.RData")
  
  sampleCorpus_from_df_min.test <- readCorpus(
    dir=sampleCorpus_TIF_df_min,
    lang="xy",
    tagger="tokenize",
    format="obj"
  )
  
  # same corrections as above
  meta(corpusTm(sampleCorpus_from_df_min.test)[[1]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpus_from_df_min.test)[[2]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpus_from_df_min.test)[[3]])[["datetimestamp"]] <-
      meta(corpusTm(sampleCorpus_from_df_min.test)[[4]])[["datetimestamp"]] <- as.POSIXlt("2020-12-08 01:01:01", tz="GMT")

  expect_equal(
    sampleCorpus_from_df_min,
    sampleCorpus_from_df_min.test
  )
})
# # new test standards
# sampleCorpus_from_df_min <- sampleCorpus_from_df_min.test
# save(sampleCorpus_from_df_min,
#   file=file.path(main.root,"tests","testthat","sampleCorpus_from_df_min.RData"),
#   compress="xz",
#   compression_level=-9)
