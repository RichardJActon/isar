% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isatab-parser.R
\name{transpose_first_to_title}
\alias{transpose_first_to_title}
\title{transpose_first_to_title}
\usage{
transpose_first_to_title(tbl)
}
\arguments{
\item{tbl}{A Tibble}
}
\value{
a tibble
}
\description{
Transposes a tibble and then used the first row of the transposed tibble
as the column names of the new tibble
}
\examples{

transpose_first_to_title(tibble::tibble(X1 = c("A a", "B B"), X2 = c(1,2)))

}
