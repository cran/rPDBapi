% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_client.R
\name{QueryGroup}
\alias{QueryGroup}
\title{Create a Grouped Query Object}
\usage{
QueryGroup(queries, logical_operator)
}
\arguments{
\item{queries}{A list of query objects to be grouped together.}

\item{logical_operator}{A string specifying the logical operator (e.g., 'AND', 'OR') to combine the queries.}
}
\value{
A list representing the grouped query object.
}
\description{
Constructs a grouped query object for performing complex searches in RCSB PDB.
It groups multiple query objects using a specified logical operator.
}
