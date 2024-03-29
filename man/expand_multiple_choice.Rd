% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kpi_data.R
\name{expand_multiple_choice}
\alias{expand_multiple_choice}
\title{Utility function that expands ODK multiple choice columns}
\usage{
expand_multiple_choice(dat, meta, delimiter = ".", drop_cols = FALSE)
}
\arguments{
\item{dat}{A data.frame, probably rbound data from the Kobo API}

\item{meta}{Associated metadata for this dataset, probably from
\link{kpi_get_xlsform}.}

\item{delimiter}{The character delimiting the parent variable name
from the child response value}

\item{drop_cols}{If \code{TRUE}, columns that have been expanded will
be dropped.}
}
\value{
\code{dat} with expanded multiple choice columns
}
\description{
For \code{select_multiple} variables, multiple responses are encoded
as space-delimited entries in each cell. This routine creates
dummy columns (1-hot encoded) for each possible response. This
is included as a step in \link{kpi_get_data}.
}
