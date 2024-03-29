% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccmppWPP_aggregate.R
\name{ccmppWPP_aggregate}
\alias{ccmppWPP_aggregate}
\title{aggregate ccmppWPP outputs across multiple location ids}
\usage{
ccmppWPP_aggregate(
  locationIDs,
  base_year,
  last_year,
  intermediate_output_folder,
  LocIDnew,
  LocNamenew
)
}
\arguments{
\item{wpp_input}{list of input objects required for one country-variant}
}
\value{
returns an "intermediate_output" list of dataframes with all ccmpp components by sex and ages 0 to 130
}
\description{
This function takes a vector of locids, compiles the ccmppWPP intermediate outputs (from ages 0:130) for
those locations, and computes the aggregate values for each demographic component
}
\details{
xx
}
\author{
Sara Hertog
}
