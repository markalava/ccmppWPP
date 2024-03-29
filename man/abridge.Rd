% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/S3_classes_demog_change_component_utility_functions.R
\name{abridge}
\alias{abridge}
\alias{abridge.demog_change_component_df}
\title{Abridge a \code{demog_change_component_df} object}
\usage{
abridge(x, ...)

\method{abridge}{demog_change_component_df}(
  x,
  age_span_abridged = NULL,
  time_span_abridged = NULL,
  age_start_abridged = NULL,
  time_start_abridged = NULL,
  out_class = class(x)[1],
  ...
)
}
\arguments{
\item{x}{An object inheriting from
\code{\link{demog_change_component_df}}.}

\item{...}{Other arguments passed to other methods.}

\item{age_span_abridged}{A numeric scalar giving the \emph{new}
\code{age_span} values; \code{x} will be abridged such that
it's \code{age_span} is \code{age_span_abridged}.}

\item{time_span_abridged}{As \code{age_span_abridged} but for time.}

\item{age_start_abridged}{Vector of \emph{new} \code{age_start}
values; \code{x} will be abridged such that
\code{age_start_abridged} defines the new age groups.}

\item{time_start_abridged}{As \code{age_start_abridged} but for
time.}

\item{out_class}{See same argument for
\code{\link{aggregate.demog_change_component_df}}.}
}
\value{
An object of the same class as \code{x}, abridged.
}
\description{
This is a generic function with methods for the various sub-classes
of \code{ccmpp_input_df}. It abridges the first argument, \code{x},
such that the result has spans or ages and times as supplied in the
\code{span_abridged}, \code{age_start_abridged} and
\code{time_start_abridged} arguments. See \dQuote{Details} for
restrictions on specifying the arguments.
}
\details{
If you specify \code{age_span_abridged} you cannot specify
\code{age_start_abridged}, and similarly for
\code{time_span_abridged} and \code{time_start_abridged}. If you
specify only one of \dQuote{age} and \dQuote{time}, all levels of
the other are kept if \code{x} has the corresponding demographic
dimension. Moreover, abridging is done within levels of \dQuote{sex} and
\dQuote{indicators} if \code{x} has those dimensions.

This function calls
\code{\link{aggregate.data.frame}} to do the actual
abridging.

If you get an error along the lines of \dQuote{result is invalid as
a member of the class} or similar, it means that, after abridging,
the result is no longer valid as a member of the class \code{x}
has. This would occur, for example, if \code{x} inherits from
\code{ccmpp_input_df} and you only supplied one of the
\code{..._start_abridged} arguments. Try supplying both or,
instead, specify \code{span_abridged}.
}
\seealso{
\code{\link[stats]{aggregate}} which this function relies
    on, \code{link{collapse_demog_dimension}} for a related
    function.
}
\author{
Mark Wheldon
}
