\name{NEWS}
\title{News for Package 'tm.plugin.koRpus'}
\encoding{UTF-8}
\section{Changes in tm.plugin.koRpus version 0.01-3 (2015-07-10)}{
  \subsection{unreleased}{
    \itemize{
      \item this release is under development
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
