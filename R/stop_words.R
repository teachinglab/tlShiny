#' Various lexicons for English stop words
#'
#' English stop words from three lexicons, as a data frame.
#' The snowball and SMART sets are pulled from the tm package. Note
#' that words with non-ASCII characters have been removed.
#'
#' @format A data frame with 1149 rows and 2 variables:
#' \describe{
#'  \item{word}{An English word}
#'  \item{lexicon}{The source of the stop word. Either "onix", "SMART", or "snowball"}
#'  }
#'
#' @source \itemize{
#' \item \url{http://www.lextek.com/manuals/onix/stopwords1.html}
#' \item \url{https://www.jmlr.org/papers/volume5/lewis04a/lewis04a.pdf}
#' \item \url{http://snowball.tartarus.org/algorithms/english/stop.txt}
#' }
"stop_words"