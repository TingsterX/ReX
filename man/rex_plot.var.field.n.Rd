% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_variation_field.R
\name{rex_plot.var.field.n}
\alias{rex_plot.var.field.n}
\title{plot function - plot the variation field map}
\usage{
rex_plot.var.field.n(
  df.plot,
  group.name = "group",
  plot.title = NULL,
  axis.min = NULL,
  axis.max = NULL,
  plot.percent = c(0, 1),
  plot.point = TRUE,
  size.point = 2,
  color.point.border = "grey50",
  color = NULL,
  color.brewer.pla = "Set2",
  plot.density = TRUE,
  alpha.density = 0.3,
  show.contour = TRUE,
  nbins.contour = 6,
  color.contour = NULL,
  show.icc_slope = TRUE,
  color.icc_slope = "grey50"
)
}
\arguments{
\item{df.plot}{dataframe for plot containing sigma2_w, sigma2_b, and group variable (can be custimized)}

\item{group.name}{character, the name of the variable that labels the differnet group variations belong to}

\item{plot.title}{title of the plot}

\item{axis.min}{the minimum value of x- and y-axis}

\item{axis.max}{the maximum value of x- and y-axis}

\item{plot.percent}{a vector - the percentile cut of the data that are shown in the plot. Default: c(0,1)}

\item{plot.point}{a Boolean value - if plot each of the input as a dot. Default: TRUE}

\item{size.point}{a integer value - the size of the dots if plot.point = TRUE}

\item{color.point.border}{a Hex code or Color name that can be read in ggplot - the color of the dots' border}

\item{color}{- a color array in color name or Hex code, e.g. c("blue", "red"). Default is NULL and use color.brewer.pla color instead}

\item{color.brewer.pla}{a palette name - palette name from ColorBrewer. check what is available RColorBrewer::display.brewer.all()}

\item{plot.density}{a Boolean value - if plot the 2d density plot (call ggplot stat_density_2d). Default:TRUE}

\item{alpha.density}{a numeric value - the alpha value if make the density plot transparent, range 0-1. Default: 0.5}

\item{show.contour}{a Boolean value - if show the contour line of the density plot}

\item{nbins.contour}{a integer value - the number of the contour lines for the density plot. Default: 6}

\item{color.contour}{a color name or Hex code - the color of the contour line. Default: "grey30"}

\item{show.icc_slope}{a Boolean value - if show the icc reference slope lines. Default: TRUE}

\item{color.icc_slope}{a color value - the color of the icc reference slope lines. Default: "grey50"}
}
\value{
ggplot plot object
}
\description{
variation field (x-axis: within-individual variation, y-axis: between-individual variation).
}
\references{
A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
}
\author{
Ting Xu
}
