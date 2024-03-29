% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_queries.R
\docType{data}
\name{match_queries}
\alias{match_queries}
\title{Expands a match_id to the home team participating in the match}
\format{
An object of class \code{list} of length 4.
}
\usage{
match_queries
}
\arguments{
\item{match_id}{Unique ID of the match to expand}
}
\value{
String of home team

String of away team




}
\description{
Expands a match_id to the home team participating in the match
}
\examples{
match_queries['home_team']['fcn'](match_id)
Expands a match_id to the away team participating in the match

match_queries['away_team']['fcn'](match_id)
Expands a match_id to the goals scored by the home team

match_queries['home_goals']['fcn'](match_id)
Expands a match_id to the goals scored by the away team

match_queries['away_goals']['fcn'](match_id)
}
\keyword{datasets}
