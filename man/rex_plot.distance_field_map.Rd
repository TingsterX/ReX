% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_distance_field.R
\name{rex_plot.distance_field_map}
\alias{rex_plot.distance_field_map}
\title{plot distance field map}
\usage{
rex_plot.distance_field_map(dFM.df, ptype = "All", point.size = 2)
}
\arguments{
\item{dFM.df}{a datafrmae containing variables: distance, wD, bD}

\item{ptype}{plot options: 'Discr', 'iRate', 'iRate2', 'Discr+iRate', 'Discr+iRate2', 'All}

\item{point.size}{dot size in the plot (default = 2)}
}
\value{
pdist: ggplot object of the distance matrix
}
\description{
plot distance field map (x-axis: within-individual distance, y-axis: between-distance distance)
}
\references{
A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
}
\author{
Ting XU
}
