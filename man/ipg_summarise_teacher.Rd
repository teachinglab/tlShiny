% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ipg_grade.R
\name{ipg_summarise_teacher}
\alias{ipg_summarise_teacher}
\title{Summarize IPG Scores Per Teacher}
\usage{
ipg_summarise_teacher(df)
}
\arguments{
\item{df}{A data frame with raw IPG rubric columns and metadata (e.g., `direct_to_ts_obs`, `ipg_rubric`).}
}
\value{
A data frame of teacher-level scores, one row per observation.
}
\description{
Converts raw IPG rubric responses to logical scores and computes summary scores by core action and domain.
}
\examples{
ipg_summarise_teacher(ipg_data)
}
