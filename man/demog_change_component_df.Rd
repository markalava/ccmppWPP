% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3_classes_demog_change_component_df.R,
%   R/S3_methods_ccmpp_input_fert_rate_df.R
\name{demog_change_component_df}
\alias{demog_change_component_df}
\alias{print.summary_fert_rate_age_f}
\title{Constructor for class \code{demog_change_component_df}}
\usage{
demog_change_component_df(
  x,
  value_type = attr(x, "value_type"),
  value_scale = attr(x, "value_scale"),
  ...
)

\method{print}{summary_fert_rate_age_f}(x, vsep, ..., print_what = c("info", "table"))
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
An object of class \code{demog_change_component_df}.
}
\description{

}
\details{
\code{demog_change_component_df} is the
parent class of a family of classes for holding age- time-specific
demographic vital rates, such as fertility and mortality rates,
population counts, and migration rates. It is not intended as a
class for raw data; only one value per age and period is
permitted. Rather, it is intended to be useful to ensure that
objects passed to functions that expect demographic values as
input are correctly formatted.

Objects of this class have class attribute
\code{c("demog_change_component_df", "data_frame")}.


The class is based on a data frame with age-sex-specific vital rates for
one or more time periods in a column named \dQuote{value}.

See the help files for the other constructor functions under
\dQuote{See also} for further details specific to each type of vital rate
or parameter.
}
\section{Class Definition}{

**TBC** (details of the class)
1. Must be sorted properly.

\subsection{Attributes of objects with this class}

**TBC**

**ISSUES OUTSTANDING**
======================

SORTING:
--------

Correct ordering by 'age_start' and 'time_start' is required for
the ccmpp functions (right?) What about by 'sex'? The sample
inputs are sorted by sex first (varying slowest) but with 'male'
first, so in reverse alphabetical order.

FACTORS vs CHARACTERS:
-----------------------

'sex' is a categorical variable and encoding it as a factor could
be justified. However it may be easier to encode as character and
have methods convert to factor when advantageous. NOTE that
ordering is important; see 'sex_as_factor()' and friends.
}

\author{
Mark Wheldon
}
\concept{demog_change_component_df constructor functions}
