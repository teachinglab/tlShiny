% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dashboard_functions.R
\name{agree_strongly_agree}
\alias{agree_strongly_agree}
\title{Agree/Strongly agree}
\usage{
agree_strongly_agree(data, question)
}
\arguments{
\item{data}{the data}

\item{question}{a string - the question to get the percentage for}
}
\value{
a string
}
\description{
Gets the percent that agree and strongly agree
}
\examples{
\dontrun{
plot_agree |>
  agree_strongly_agree(question = "x")
}
}
