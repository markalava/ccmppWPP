% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lt_helper_functions_wpp.R
\name{lt_complete_loop_over_time}
\alias{lt_complete_loop_over_time}
\title{Compute complete life tables for multiple time periods}
\usage{
lt_complete_loop_over_time(mx, sex, a0rule = "ak", OAnew = 130)
}
\arguments{
\item{mx}{data frame. "value" column contains age-specific mortality rates by time_start and age_start for one sex.}

\item{sex}{character. string indicating "m" male, "f" female, or "b" both sexes.}

\item{a0rule}{character. string "ak" for andreev-kinkaid and "cd" for coale-demeny estimation of 1a0.}

\item{OAnew}{numeric. The starting age for the new open age group.}
}
\value{
a data frame with "indicator" labeling the life table column name and "value" containing the value of 
that measure, by single year of age_start and time_start
}
\description{
This function computes complete life table values, looping over time periods and using the 
lt_single_mx() function in DemoTools.
}
\author{
Sara Hertog
}
