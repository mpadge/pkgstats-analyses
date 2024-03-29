% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_new_vs_update}
\alias{plot_new_vs_update}
\title{Plot time series of rates of new submissions versus updated packages}
\usage{
plot_new_vs_update(
  datafile = "pkgstats-results.Rds",
  bimonthly = FALSE,
  start_date = "2018-01-01",
  type = "lines",
  lwd = 1
)
}
\arguments{
\item{datafile}{Name of local file containing data to load}

\item{bimonthly}{If \code{TRUE}, aggregate data first into bimonthly intervals
(which generally produces a nicer looking plot).}

\item{start_date}{First date to display, or set to \code{NULL} to display full
range.}

\item{type}{Either "bars" for a bar (column) graph, or "lines" for a line
graph.}

\item{lwd}{For type = "lines" only.}
}
\description{
Plot time series of rates of new submissions versus updated packages
}
