% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_catalog.R
\name{get_catalog}
\alias{get_catalog}
\title{Get catalog of available data}
\usage{
get_catalog(
  dataset = c("full", "ene", "epf_personas", "epf_gastos", "enusc", "esi")
)
}
\arguments{
\item{dataset}{\code{string} name of dataset. The possible values are "full", "ene", "epf_personas", "epf_gastos", "enusc" or "esi". The default value is "full"}
}
\value{
\code{dataframe with two columns: survey and version}
1. Columns

\itemize{
  \item \code{survey} One of the surveys conducted by INE Chile
  \item \code{version} Specific version of a survey. It can be year or a quarter
}
}
\description{
Get catalog of available data
}
\examples{
# Get all datasets and versions available
get_catalog()
# Get all versions of a specific dataset
get_catalog(dataset="enusc")
}
