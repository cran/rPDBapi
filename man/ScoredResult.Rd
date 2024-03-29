% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_client.R
\name{ScoredResult}
\alias{ScoredResult}
\title{Create a Scored Result Object}
\usage{
ScoredResult(entity_id, score)
}
\arguments{
\item{entity_id}{A string representing the entity ID.}

\item{score}{A numeric value representing the score associated with the entity.}
}
\value{
A list representing the scored result.
}
\description{
Constructs a scored result object typically used in search results to associate an entity ID with its score.
}
