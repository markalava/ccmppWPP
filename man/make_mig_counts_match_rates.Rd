% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3_classes_ccmpp_input_mig_net_rate_df.R
\name{make_mig_counts_match_rates}
\alias{make_mig_counts_match_rates}
\title{Make migration counts and rates match in a CCMPP input list}
\usage{
make_mig_counts_match_rates(x, set_mig_type = TRUE)
}
\arguments{
\item{x}{A \code{\link{ccmpp_input_list}}}

\item{set_mig_type}{Logical; should \code{\link{mig_type}(x)} be set to \code{"rates"}?}
}
\value{
\code{x} suitably modifed.
}
\description{
The \code{\link{mig_net_count_age_sex}} and
\code{\link{mig_net_rate_age_sex}} components of a
\code{\link{ccmpp_input_list}} can be set independentely of each
other. When a projection is done, the \code{\link{mig_parameter}}
component determines whether the rates or counts are used. This
function will modify its first argument by overwriting the
\code{\link{mig_net_count_age_sex}} component with new counts such
the rates and counts yield identical projections.
}
\seealso{
Other migration rate functions:
    \code{\link{mig_net_rate_age_sex}},
    \code{\link{mig_parameter}}, \code{\link{mig_type}}. Also
    \code{\link{ccmpp_input_list}} and
    \code{\link{pop_count_age_sex}} for obtaining population
    projections.
}
\author{
Mark Wheldon
}
