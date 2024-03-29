% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_calc.R
\name{fingerprinting_wraper}
\alias{fingerprinting_wraper}
\title{Reliability Explorer wrap-up function - fingerprinting}
\usage{
fingerprinting_wraper(
  data,
  sID,
  visit,
  method.dist = "euclidean",
  method.FP = 1,
  shiny_progress_bar = FALSE
)
}
\arguments{
\item{data}{[n x p] data matrix: n observations x p variables}

\item{sID}{[n x 1] a data frame containing subject ID}

\item{visit}{[n x 1] a data frame containing the repetition identify (e.g. time1, time2)}

\item{method.dist}{[1] a character value indicating the method to calculate the distance matrix (default: "euclidean"), check \code{dist}}

\item{method.FP}{[1] a numeric value indicating which method used to count the identification rate (default: 1). Check \code{calc_fingerprinting}}

\item{shiny_progress_bar}{[1] a Boolean value - show the Shiny progress bar if this function is called in shiny app (shinybusy library is required)}
}
\value{
out_list: a list containing identification rate (fingerprinting) for each variable
}
\description{
A function for computing the fingerprinting for each row variable in data (show progress bar in shiny)
}
\references{
A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
}
\author{
Ting XU
}
