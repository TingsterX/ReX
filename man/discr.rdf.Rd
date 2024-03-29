% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discriminability.R
\name{discr.rdf}
\alias{discr.rdf}
\title{Discriinability Density Function}
\usage{
discr.rdf(dist, ids)
}
\arguments{
\item{dist}{[n, n]: a distance matrix for n subjects.}

\item{ids}{[n]: a vector containing the subject ids for each subject.}
}
\value{
rdf [n]: the reliability per subject.
}
\description{
A function for computing the reliability density function of a dataset.
}
\author{
Shangsi Wang, Eric Bridgeford and Gregory Kiar
}
