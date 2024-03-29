% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_read.R
\name{data_read}
\alias{data_read}
\title{Read (import) data files from various sources}
\usage{
data_read(path, rowname = TRUE, colname = TRUE, sheet = 1, verbose = TRUE, ...)
}
\arguments{
\item{path}{Character string, the file path to the data file.}

\item{rowname}{See read.table row.names}

\item{colname}{See read.table col.names}

\item{sheet}{The name or index of the sheet to read data from.}

\item{verbose}{Toggle warnings and messages.}

\item{...}{Arguments passed to the related `read_*()` function.}
}
\description{
Read (import) data files from various sources
}
