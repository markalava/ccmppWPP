% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3_classes_ccmpp_output_pop_count_age_sex_df.R
\name{new_pop_count_age_sex}
\alias{new_pop_count_age_sex}
\title{Low-level constructor for class \code{pop_count_age_sex}.}
\usage{
new_pop_count_age_sex(
  x,
  age_span = double(),
  time_span = 0,
  dimensions = get_req_dimensions_for_subclass_classes("pop_count_age_sex"),
  value_type = get_value_types_for_subclass_classes("pop_count_age_sex"),
  value_scale = double(),
  ...,
  class = character()
)
}
\arguments{
\item{x}{For \code{demog_change_component_df}: A data frame with
columns \dQuote{age_start}, \dQuote{age_span}, \dQuote{sex},
\dQuote{time_start}, \dQuote{time_span}, and \dQuote{value}.}

\item{value_type}{Scalar indicating the type of the \dQuote{value}
column (e.g., \dQuote{count}, \dQuote{rate}, etc.).}

\item{value_scale}{\emph{Numeric} scalar indicating the value_scale of the
counts, e.g., \code{1}, \code{1000}, \code{1e6}, etc.}

\item{...}{Passed to the low-level constructor.}
}
\value{
An object of class \code{pop_count_age_sex}.
}
\description{
Creates an object of class \code{pop_count_age_sex}. Minimal
checks are done; for interactive use see
\code{\link{pop_count_age_sex}}.

This function is not exported. The user-level constructor is
\code{\link{pop_count_age_sex}}.
}
\section{Note}{

There is no \code{age_span} argument. Population counts
    have time span 0.
}

\seealso{
pop_count_age_sex
}
\author{
Mark Wheldon
}
