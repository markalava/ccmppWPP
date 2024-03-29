% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccmppWPP_input_file.R
\name{ccmpp_input_file_proj_variants}
\alias{ccmpp_input_file_proj_variants}
\title{Derive inputs needed for deterministic projection variants}
\usage{
ccmpp_input_file_proj_variants(
  ccmppWPP_estimates,
  ccmppWPP_medium,
  PasfrGlobalNorm
)
}
\arguments{
\item{medium_variant_outputs}{list of data frames. medium variant output from the ccmppWPP_workflow_one_country_variant function}
}
\value{
a list of data frames
}
\description{
Returns a list of age specific fertility rates and life table values needed as inputs to the
deterministic projection variants, including low, high and constant fertility, instant replacement fertility and
momentum, and constant mortality
}
\details{
accesses the global model in the data frame "pasfr_global_model" needed to infer pasfr from tfr
}
\author{
Sara Hertog
}
