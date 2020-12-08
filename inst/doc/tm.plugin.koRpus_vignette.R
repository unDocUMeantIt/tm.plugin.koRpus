## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
header_con <- file("vignette_header.html")
writeLines('<meta name="flattr:id" content="4zdzgd" />', header_con)
close(header_con)

## ---- include=FALSE, cache=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load knitr for better graphics support
library(knitr)

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  library(tm.plugin.koRpus)
#  library(koRpus.lang.de)
#  # set the root path to the sample files
#  sampleRoot <- file.path(path.package("tm.plugin.koRpus"), "tests", "testthat", "samples")
#  # the next call uses "hierarchy" to describe the directory structure
#  # and its meaning; see below
#  sampleTexts <- readCorpus(
#    dir=sampleRoot,
#    hierarchy=list(
#      Topic=c(
#        C3S="C3S SCE",
#        GEMA="GEMA e.V."
#      ),
#      Source=c(
#        Wikipedia_alt="Wikipedia (alt)",
#        Wikipedia_neu="Wikipedia (neu)"
#      )
#    ),
#    tagger="tokenize",
#    lang="de"
#  )

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  allC3SSources <- corpusSources(corpusTopics(sampleTexts, "C3S"))
#  names(allC3SSources)

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  sampleTexts <- lex.div(sampleTexts)
#  corpusSummary(sampleTexts)

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  library(sciplot)
#  lineplot.CI(
#    x.factor=sampleTexts[["Source"]],
#    response=sampleTexts[["MTLD"]],
#    group=sampleTexts[["Topic"]],
#    type="l",
#    main="MTLD",
#    xlab="Media source",
#    ylab="Lexical diversity score",
#    col=c("grey", "black"),
#    lwd=2
#  )

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  sampleTexts <- read.corp.custom(sampleTexts, caseSens=FALSE)
#  sampleTextsWordFreq <- query(
#    corpusFreq(sampleTexts)[["corpus"]],
#    var="wclass",
#    query=kRp.POS.tags(lang="de", list.classes=TRUE, tags="words")
#  )
#  head(sampleTextsWordFreq, 10)

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  require(wordcloud)
#  colors <- brewer.pal(8, "RdGy")
#  wordcloud(
#    head(sampleTextsWordFreq[["word"]], 200),
#    head(sampleTextsWordFreq[["freq"]], 200),
#    random.color=TRUE,
#    colors=colors
#  )

