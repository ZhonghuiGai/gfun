% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme.R
\name{theme_prism}
\alias{theme_prism}
\title{GraphPad Prism themes}
\usage{
theme_prism(
  base_size = 16,
  angle = 45,
  border = FALSE,
  plot.margin = c(3, 3, 3, 3)
)
}
\arguments{
\item{base_size}{`numeric`. Base font size, given in `"pt"`.}

\item{angle}{`integer`. Angle of x_axis text in degrees.
One of: `-30, 45, 90, 270`.}

\item{border}{`logical`. Should a border be drawn around the plot?
Clipping will occur unless e.g. `coord_cartesian(clip = "off")` is used.}

\item{plot.margin}{plot.margin, trouble, mm}
}
\value{
Returns a list-like object of class _theme_.
}
\description{
GraphPad Prism themes
}
