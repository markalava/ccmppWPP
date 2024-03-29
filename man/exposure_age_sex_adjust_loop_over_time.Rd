% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exposure_helper_functions.R
\name{exposure_age_sex_adjust_loop_over_time}
\alias{exposure_age_sex_adjust_loop_over_time}
\title{Loop over time to adjust age-specific person years of exposure
THIS FUNCTION IS NOT LONGER USED.  NEED TO UPDATE DOCUMENTATION TO REMOVE IT FROM NAMESPACE AND PREVENT ERRORS
ON PACKAGE INSTALLATION
WHEN I TRY TO UPDATE DOCUMENTION, IT GIVES ME A BUNCH OF ERRORS WRT DOCUMENATION OF S3 CLASSES
NEED TO CHECK WITH MARK ABOUT WHAT IS GOING WRONG HERE}
\usage{
exposure_age_sex_adjust_loop_over_time(death_age_sex_period, mx)
}
\arguments{
\item{death_age_sex_period}{data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
the sex- and age-specific death counts output by the death_age_sex_loop_over_time function.}

\item{mx}{data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
the original sex- and age-specific mortality rates on the ccmpp_input file.}
}
\value{
a data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
the sex- and age-specific person years of exposure counts
}
\description{
Loops over time to re-compute age-specific exposure from input age_specific mortality and age-specific deaths
periods of time.
}
\author{
Sara Hertog
}
