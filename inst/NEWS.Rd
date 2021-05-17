\name{NEWS}
\title{News for Package 'tm.plugin.koRpus'}
\encoding{UTF-8}
\section{Changes in tm.plugin.koRpus version 0.4-2 (2021-05-17)}{
  \subsection{fixed}{
    \itemize{
      \item updated test standards after changes to koRpus' internal calculations
        of numer of lines in texts imported from TIF data frames
    }
  }
  \subsection{changed}{
    \itemize{
      \item kRp.corpus: replaced \code{prototype()} in class definition with initialize
        method
    }
  }
}
\section{Changes in tm.plugin.koRpus version 0.4-1 (2020-12-17)}{
  \subsection{fixed}{
    \itemize{
      \item \code{docTermMatrix()}: results were wrong because numbers were assigned to
        wrong columns; now fixed in koRpus
      \item unit tests failed on windows due to an UTF-8 issue
    }
  }
  \subsection{changed}{
    \itemize{
      \item the nested object class kRp.hierarchy was replaced by kRp.corpus; instead
        of reproducing the file hierarchy in the object structure, kRp.corpus has
        a flat structure with all texts in one single data frame; this data frame
        was also renamed from \code{"TT.res"} into \code{"tokens"} the class name kRp.corpus
        was used in tm.plugin.koRpus before and is just being recycled ;) kRp.corpus
        inherits from class kRp.text as defined in the koRpus package
      \item status messages are currently only shown when only one CPU is used
      \item \code{corpusTagged()}: now called \code{taggedText()} as in koRpus
      \item \code{corpusDesc()}: now called \code{describe()} as in koRpus
      \item [, [<-, [[ and [[<- methods no longer apply to the summary data frame but
        tokens slot as in koRpus (where it applies to the TT.res slot)
      \item \code{show()}: kRp.corpus objects now list all available features
      \item \code{read.corp.custom()}: removed unused mc.cores argument
      \item \code{docTermMatrix()}: by default behaves like most other methods and adds its
        result to the input object rather than returning just the matrix; also,
        the generic is now defined by the koRpus package and was removed, including
        all of the actual function code
      \item adjusted unit tests and vignette
      \item updated all examples to use a new sample corpus (see added), to the
        benefit that many "\\dontrun\{\}" cases could be removed
    }
  }
  \subsection{added}{
    \itemize{
      \item \code{readCorpus()}: the hierarchy levels of a text corpus can now be assumed
        directly from the directory structure by setting "hierarchy=TRUE"
      \item \code{corpusHasFeatures()}, \code{corpusHasFeatures()}<-, \code{corpusFeatures()},
        \code{corpusFeatures()}<-, \code{corpusHierarchy()}, \code{corpusHierarchy()}<-, \code{corpusCorpFreq()},
        \code{corpusCorpFreq()}<-, \code{diffText()}, \code{diffText()}<-, \code{originalText()}: new getter/setter
        methods for kRp.corpus objects
      \item \code{split_by_doc_id()}: new method transforms a kRp.corpus object into a list
        of kRp.text objects
      \item \code{corpusDocTermMatrix()}: new method to get/set the sparse document term
        matrix in kRp.corpus objects
      \item [[/[[<-: gained new argument \code{"doc_id"} to limit the scope to particular
        documents
      \item \code{describe()}/describe()<-: now support filtering by doc_id
      \item new sample corpus for use in examples
    }
  }
  \subsection{removed}{
    \itemize{
      \item removed all classes and methods dealing with kRp.hierarchy
      \item removed deprecated methods of the pre-kRp.hierarchy era
      \item removed generic of \code{tif_as_tokens_df()} as it was moved to the koRpus
        package
    }
  }
}
\section{Changes in tm.plugin.koRpus version 0.3-1 (2019-05-14)}{
  \subsection{fixed}{
    \itemize{
      \item \code{readCorpus()}: solved a cryptic warning when more than one text was
        tokenized
    }
  }
  \subsection{added}{
    \itemize{
      \item \code{docTermMatrix()}: new method to generate document-term matrices, either
        with absolute frequencies or tf-idf values
      \item \code{query()}: new method, extending the generic of koRpus >= 0.12-1
      \item \code{filterByClass()}: new method, extending the generic of koRpus >= 0.12-1
      \item \code{jumbleWords()}: new method, extending the generic of koRpus >= 0.12-1
      \item \code{clozeDelete()}: new method, extending the generic of koRpus >= 0.12-1
      \item \code{cTest()}: new method, extending the generic of koRpus >= 0.12-1
      \item \code{textTransform()}: new method, extending the generic of koRpus >= 0.12-1
      \item \code{show()}: new method for objects of class kRp.hierarchy
    }
  }
  \subsection{changed}{
    \itemize{
      \item depends on koRpus >= 0.12-1 now
      \item depends on the Matrix package now (for \code{docTermMatrix()})
      \item adjusted test standards to include the additional POS tags from koRpus >=
        0.12-1
    }
  }
}
\section{Changes in tm.plugin.koRpus version 0.02-2 (2019-01-18)}{
  \subsection{fixed}{
    \itemize{
      \item \code{readCorpus()}, \code{kRpSource()}: added missing imports from packages tm, NLP
        and parallel
      \item \code{readCorpus()}: fixed status message formatting
      \item \code{corpusTm()}: removed useless \code{"level"} argument and corrected the output
      \item \code{readCorpus()}: removed unused \code{"level"} argument
      \item \code{corpusFiles()}: now also works with flat hierarchy objects
    }
  }
  \subsection{added}{
    \itemize{
      \item \code{readCorpus()}: can now also import data frames in TIF format, including
        support for hierarchal categories
      \item \code{tif_as_corpus_df()}: new S4 method to transform a kRp.hierarchy object
        into a TIF compliant data frame
    }
  }
  \subsection{changed}{
    \itemize{
      \item \code{readCorpus()}: the tm corpora now include full hierarchy metadata
      \item removed pre-hierarchy portions from internal function \code{whatIsAvailable()}
    }
  }
}
\section{Changes in tm.plugin.koRpus version 0.02-1 (2018-07-29)}{
  \subsection{changed}{
    \itemize{
      \item vignette: also includes info on \code{readCorpus()}
      \item tests: adjusted test standards to new object class
    }
  }
  \subsection{added}{
    \itemize{
      \item kRp.hierarchy: new S4 class to replace kRp.sourcesCorpus and
        kRp.topicCorpus to allow more generic nesting of hierarchical levels
      \item \code{readCorpus()}: new function to generate kRp.hierarchy objects recursively
      \item many corpus*() getter functions can now filter by hierarchy level or
        category ID
      \item removed all code regarding \code{simpleCorpus()}, \code{sourcesCorpus()} and
        \code{topicCorpus()}, their object classes and methods; this is all handled much more
        flexible by kRp.hierarchy and \code{readCorpus()} now
    }
  }
}
\section{Changes in tm.plugin.koRpus version 0.01-4 (2018-03-07)}{
  \subsection{fixed}{
    \itemize{
      \item \code{sourcesCorpus()}: speak of \code{"text"} instead of \code{"texts"} if it's only one
    }
  }
  \subsection{changed}{
    \itemize{
      \item adjusted package to support koRpus >= 0.11 and sylly, especially with
        regards to \code{summary()}, \code{hyphen()}, and new class contructors
      \item \code{summary()}: for more coherence with the koRpus package the \code{"text"} column
        in the summary slot was renamed into \code{"doc_id"}
      \item reaktanz.de supports HTTPS now, updated references
      \item vignette is now in RMarkdown/HTML format; the SWeave/PDF version was
        dropped
      \item \code{hyphen()}/lex.\code{div()}/readability(): 'quiet' is now TRUE by default
      \item \code{lex.div()}: 'char' is now an emtpy string by default; computing all
        characteristics was not a useful default for large text corpora
    }
  }
  \subsection{added}{
    \itemize{
      \item README.md
      \item new [, [<-, [[ and [[<- methods added for corpus object classes
      \item new methods \code{tif_as_tokens_df()} to export corpus objects as a single
        data.frame in fully TIF compliant format
      \item \code{summary()}: now also includes the total number of stopwords (if available)
      \item new class object contructors \code{kRp_corpus()}, \code{kRp_sourcesCorpus()}, and
        \code{kRp_topicCorpus()} can be used instead of new(\code{"kRp.corpus"}, ...) etc.
    }
  }
}
\section{Changes in tm.plugin.koRpus version 0.01-3 (2016-07-12)}{
  \subsection{fixed}{
    \itemize{
      \item the arguments that \code{simpleCorpus()} was supposed to pipe to \code{DirSource()}
        weren't used
    }
  }
  \subsection{changed}{
    \itemize{
      \item the \code{"paths"} argument of \code{topicCorpus()} now expects a list, not a vector
      \item using the parallel package to be able to use more CPU cores
    }
  }
  \subsection{added}{
    \itemize{
      \item new argument \code{"format"} for \code{simpleCorpus()}, \code{sourceCorpus()}, and
        \code{topicCorpus()}, to be able to work with text objects directly, instead of files
    }
  }
}
\section{Changes in tm.plugin.koRpus version 0.01-2 (2015-07-08)}{
  \subsection{changed}{
    \itemize{
      \item using the S4 methods of koRpus 0.06-1 now, therefore renamed all methods
        removing the *.corpus suffix (e.g., \code{lex.div.corpus()} is now \code{lex.div()})
      \item renamed classes into kRp.corpus, kRp.sourcesCorpus and kRp.topicCorpus,
        and their generator functions accordingly
    }
  }
  \subsection{added}{
    \itemize{
      \item new methods \code{read.corp.custom()}, \code{freq.analysis()} and \code{summary()}
      \item new getter/setter methods: \code{corpusSources()}, \code{corpusTopics()}, \code{corpusFreq()},
        \code{corpusSummary()}
      \item first basic unit tests, using the testthat package
      \item new option \code{"summary"} for \code{lex.div()} and \code{readability()}, to automatically
        update the summary data.frames
      \item first notes in a vignette
    }
  }
}
\section{Changes in tm.plugin.koRpus version 0.01-1 (2015-06-29)}{
  \subsection{added}{
    \itemize{
      \item initial release
    }
  }
}
