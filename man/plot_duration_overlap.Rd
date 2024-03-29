% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plots.R
\name{plot_duration_overlap}
\alias{plot_duration_overlap}
\title{Plot and save overlap of duration between all mice permutations}
\usage{
plot_duration_overlap(
  data,
  plot_file_name,
  time_lower_limit,
  nr_mice_in_group,
  plot_height,
  plot_width,
  DPI = 150
)
}
\arguments{
\item{data}{Data frame obtained by using \code{\link{get_duration_overlap}}}

\item{plot_file_name}{File name to create on disk}

\item{time_lower_limit}{Minimum number of seconds of overlap to be considered.}

\item{nr_mice_in_group}{Minimum Number of mice should beD grouped together to calculate the overlapped duration}

\item{plot_height}{Plot height}

\item{plot_width}{Plot width}

\item{DPI}{Plot resolution(Dots per Inch)}
}
\description{
The function plot the overlap value (time spent in same cages in seconds) as bubbles. The larger the bubble, the higher the overlap value is
The figure is saved as a PNG file in the result environment.
}
