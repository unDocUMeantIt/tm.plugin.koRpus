\name{NEWS}
\title{News for Package 'tm.plugin.koRpus'}
\encoding{UTF-8}
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
