% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3_classes_validators_helpers.R
\name{tabulate_lexis_squares}
\alias{tabulate_lexis_squares}
\title{Tabulate lexis squares}
\usage{
tabulate_lexis_squares(x)
}
\arguments{
\item{x}{A data frame.}
}
\value{
A table.
}
\description{
For each cell on the lexis plane (e.g., each age-time-sex 'year')
tabulate the number of observations that provide information on
it. This should be exactly 1 for a valid CCMPP input data frame
because CCMPP requires one (and only one) value per cell.
}
\details{
This function was written as a validation tool. It is currenlty not
used because valid \code{\link{ccmpp_input_df}} objects require all
spans to be the same and for them to equal the row-wise differences
between the corresponding \dQuote{\code{_start}} columns (e.g.,
\code{age_span[i]} must equal \code{age_start[i] - age_start[i-1]}
for 1 < \code{i} <= \code{nrow(x)}). In this case, there
}
\author{
Mark Wheldon
}
