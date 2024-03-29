% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_distance_field.R
\name{rex_plot.discri_matrix}
\alias{rex_plot.discri_matrix}
\title{plot Discriminability for all observation and variables}
\usage{
rex_plot.discri_matrix(
  DiscrSub,
  subID,
  rep,
  gtitle = "",
  cmap = c("white", "blue")
)
}
\arguments{
\item{DiscrSub}{[n x p] Discriminability for each observation and variable(s)}

\item{subID}{[n] a vector for subject ID, row should match the Discriminability input}

\item{rep}{[n] a vector for repetition, row should match the Discriminability input}

\item{gtitle}{plot title (default: NULL)}

\item{cmap}{palette name from ColorBrewer, or a color array, e.g. c("blue", "red"). By default, color=c("white", "red)
Check out the what color palette available
\code{library(ColorBrewer); display.brewer.all()}}
}
\value{
ggplot object of the distance matrix
}
\description{
plot Discriminability for all observation and variables - ggplot based
}
\references{
A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
}
\author{
Ting XU
}
