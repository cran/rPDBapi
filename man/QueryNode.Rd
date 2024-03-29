% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_client.R
\name{QueryNode}
\alias{QueryNode}
\title{Create a Query Node Object}
\usage{
QueryNode(search_operator, logical_operator = NULL)
}
\arguments{
\item{search_operator}{A search operator or group object.}

\item{logical_operator}{A string specifying the logical operator, default is NULL. Used only if the search_operator is a group.}
}
\value{
A list representing the query node.
}
\description{
Constructs a query node, which can be a terminal node or a grouped node, for complex RCSB PDB searches. This function is used to structure queries for the search system.
}
