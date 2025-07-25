% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tl_themes.R
\name{theme_tl}
\alias{theme_tl}
\title{Teaching Lab Custom Ggplot2 Theme}
\usage{
theme_tl(
  base_family = "Lora",
  base_size = 14,
  strip_text_family = base_family,
  strip_text_size = 15,
  plot_title_family = "Poppins",
  plot_title_size = 20,
  plot_title_margin = 10,
  subtitle_family = "Lora",
  subtitle_size = 15,
  subtitle_margin = 15,
  caption_family = "Lora",
  caption_size = 11,
  caption_margin = 10,
  axis_title_family = "Poppins",
  axis_title_size = 12,
  axis_title_just = "mm",
  axis_text_size = 10.5,
  dark = FALSE,
  grid = TRUE,
  axis = FALSE,
  ticks = FALSE,
  markdown = FALSE,
  legend = F
)
}
\arguments{
\item{base_family}{base font family}

\item{base_size}{base font size}

\item{strip_text_family}{facet label font family}

\item{strip_text_size}{facet label text size}

\item{plot_title_family}{plot title family}

\item{plot_title_size}{plot title font size}

\item{plot_title_margin}{plot title margin}

\item{subtitle_family}{plot subtitle family}

\item{subtitle_size}{plot subtitle size}

\item{subtitle_margin}{plot subtitle margin}

\item{caption_family}{plot caption family}

\item{caption_size}{plot caption size}

\item{caption_margin}{plot caption margin}

\item{axis_title_family}{axis title font family}

\item{axis_title_size}{axis title font size}

\item{axis_title_just}{axis title font justification \code{blmcrt}}

\item{axis_text_size}{axis text size}

\item{dark}{dark mode \code{TRUE}, \code{FALSE}}

\item{grid}{panel grid (\code{TRUE}, \code{FALSE}, or a combination of
\code{X}, \code{x}, \code{Y}, \code{y})}

\item{axis}{axis \code{TRUE}, \code{FALSE}, [\code{xy}]}

\item{ticks}{ticks \code{TRUE}, \code{FALSE}}

\item{markdown}{enabled ggtext markdown styling  \code{TRUE}, \code{FALSE}}

\item{legend}{default no legend with F}
}
\description{
Teaching Lab Custom Ggplot2 Theme
}
\note{
It requires installing Poppins, Lora fonts unless you change the font parameters

\url{https://www.google.com/fonts}
}
