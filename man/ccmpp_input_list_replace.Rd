% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3_methods_ccmpp_input_list.R
\name{ccmpp_input_list_replace}
\alias{ccmpp_input_list_replace}
\alias{$<-.ccmpp_input_list}
\alias{[[<-.ccmpp_input_list}
\title{Replace Parts of a \code{ccmpp_input_list}}
\usage{
\method{$}{ccmpp_input_list}(x, name) <- value

\method{[[}{ccmpp_input_list}(x, i, j) <- value
}
\arguments{
\item{x}{data frame.}

\item{name}{
    A literal character string or a \link[base]{name} (possibly \link[base]{backtick}
    quoted).}

\item{value}{A suitable replacement value: it will be repeated a whole
    number of times if necessary and it may be coerced: see the
    Coercion section.  If \code{NULL}, deletes the column if a single
    column is selected.}

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
}
\value{
A \code{list}.
}
\description{
These are methods for the subset-replacement functions for objects
of class \code{ccmpp_input_list}. The return values differ and,
importantly, are not objects of class \code{ccmpp_input_list} (see
\dQuote{Details}).
}
\details{
Arbitrary modifications of \code{ccmpp_input_list}s may not be
valid members of the class. Therefore, the object resulting from a
subset operation via \code{`[`}, or a replacement via \code{`[<-`},
and similar, will no longer inherit from class
\code{ccmpp_input_list} (or subclasses) and the attributes specific
to those classes will be lost.
}
\seealso{
\code{\link{ccmpp_input_list}} for class definitions,
    \code{link{subset_time}} and friends, and
    \code{\link{[.demog_change_component_df}} for similar functions
    for the constituent data frame objects.
}
\author{
Mark Wheldon
}
