% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discriminability.R
\name{calc_discriminability}
\alias{calc_discriminability}
\title{calculate the discriminability}
\usage{
calc_discriminability(dist, sID, all_discr.return = FALSE)
}
\arguments{
\item{dist}{[n x n]: distance matrix.}

\item{sID}{[n]: a subject ID vector}

\item{all_discr.return}{[1]: if return all discriminaiblity for each within-individual repetition}
}
\value{
d.all: discriminability (D) for the entire dataset
If all_discr.return = TRUE, a list is returned, containing D for the entire dataset and D for each repetition
}
\description{
call discriminability function to calculate the discriminability \code{discr.discr}
}
