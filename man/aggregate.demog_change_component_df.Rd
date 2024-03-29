% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3_methods_demog_change_component_df.R
\name{aggregate.demog_change_component_df}
\alias{aggregate.demog_change_component_df}
\title{Aggregate a \code{demog_change_component_df}}
\usage{
\method{aggregate}{demog_change_component_df}(x, by, FUN, ..., simplify = TRUE, drop = TRUE)
}
\arguments{
\item{x}{an R object.}

\item{by}{a list of grouping elements, each as long as the variables
    in the data frame \code{x}.  The elements are coerced to factors
    before use.}

\item{FUN}{a function to compute the summary statistics which can be
    applied to all data subsets.}

\item{...}{further arguments passed to or used by methods.}

\item{simplify}{a logical indicating whether results should be
    simplified to a vector or matrix if possible.}

\item{drop}{a logical indicating whether to drop unused combinations
    of grouping values.  The non-default case \code{drop=FALSE} has been
    amended for \R 3.5.0 to drop unused combinations.}
}
\description{
A method for the generic \code{\link[stats]{aggregate}}
function. This merely calls the
\code{\link[stats]{aggregate.data.frame}} method and issues a
warning that the class will be dropped (if called
interactively). See \code{link{collapse_demog_dimension}} for an
alternative if aggregation across an entire demographic dimension
(e.g., \dQuote{age}, \dQuote{time}, etc.) is desired.
}
\seealso{
\code{\link[stats]{aggregate}}, \code{link{collapse_demog_dimension}}
}
\author{
Mark Wheldon
}
