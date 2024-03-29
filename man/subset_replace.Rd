% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3_methods_demog_change_component_df.R
\name{subset_replace}
\alias{subset_replace}
\alias{[.demog_change_component_df}
\alias{[<-.demog_change_component_df}
\alias{$<-.demog_change_component_df}
\alias{[[<-.demog_change_component_df}
\alias{subset.demog_change_component_df}
\title{Extract or Replace Parts of a \code{demog_change_component_df}}
\usage{
\method{[}{demog_change_component_df}(x, i, j, drop)

\method{[}{demog_change_component_df}(x, i, j) <- value

\method{$}{demog_change_component_df}(x, name) <- value

\method{[[}{demog_change_component_df}(x, i, j) <- value

\method{subset}{demog_change_component_df}(x, ...)
}
\arguments{
\item{x}{data frame.}

\item{i}{elements to extract or replace.  For \code{[} and
    \code{[[}, these are \code{numeric} or \code{character} or, for
    \code{[} only, empty or \code{logical}.  Numeric values are coerced to integer as if
    by \code{\link[base]{as.integer}}.  For replacement by \code{[}, a logical
    matrix is allowed.}

\item{j}{elements to extract or replace.  For \code{[} and
    \code{[[}, these are \code{numeric} or \code{character} or, for
    \code{[} only, empty or \code{logical}.  Numeric values are coerced to integer as if
    by \code{\link[base]{as.integer}}.  For replacement by \code{[}, a logical
    matrix is allowed.}

\item{drop}{logical.  If \code{TRUE} the result is coerced to the
    lowest possible dimension.  The default is to drop if only one
    column is left, but \bold{not} to drop if only one row is left.}

\item{value}{A suitable replacement value: it will be repeated a whole
    number of times if necessary and it may be coerced: see the
    Coercion section.  If \code{NULL}, deletes the column if a single
    column is selected.}

\item{name}{
    A literal character string or a \link[base]{name} (possibly \link[base]{backtick}
    quoted).}

\item{...}{elements to extract or replace.  For \code{[} and
    \code{[[}, these are \code{numeric} or \code{character} or, for
    \code{[} only, empty or \code{logical}.  Numeric values are coerced to integer as if
    by \code{\link[base]{as.integer}}.  For replacement by \code{[}, a logical
    matrix is allowed.}
}
\value{
A \code{data.frame},
    one-dimensional vector, or scalar depending on the dimensions
    of the extracted values.
}
\description{
These are methods for the subset operators and their companion
replacement functions for objects of class
\code{demog_change_component_df}. The return values differ and,
importantly, subsetting and subset-replacement do not return objects of class
\code{demog_change_component_df} (see \dQuote{Details}).
}
\details{
Arbitrary subsets or modifications of
\code{demog_change_component_df}s (and objects of subclasses) may
not be valid members of the class. Therefore, the object resulting
from a subset operation via \code{`[`}, or a replacement via
\code{`[<-`}, and similar, will no longer inherit from class
\code{demog_change_component_df} (or subclasses) and the attributes
specific to those classes will be lost.

To create a \code{demog_change_component_df} object from a subset
call \code{\link{demog_change_component_df}} explicitly (e.g.,
\code{demog_change_component_df(x[x$time_start == 1960,])}). A
similar approach can be taken for subset-replacement. For an
alternative approach to simple subsetting on the demographic change
component dimensions see \code{\link{subset_time}} and friends.
}
\seealso{
\code{\link{demog_change_component_df}} for class
    definitions, \code{link{subset_time}} and friends.
}
\author{
Mark Wheldon
}
