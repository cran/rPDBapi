% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_graphql.R
\name{search_graphql}
\alias{search_graphql}
\title{Perform a GraphQL Query to RCSB PDB}
\usage{
search_graphql(graphql_json_query)
}
\arguments{
\item{graphql_json_query}{A list containing the GraphQL query in JSON format.}
}
\value{
The parsed content of the response from the PDB.
}
\description{
Executes a GraphQL query against the RCSB Protein Data Bank (PDB).
It sends the query in JSON format and handles the HTTP response, including error checking.
}
\examples{
graphql_json_query <- list(query = "{entries(entry_ids:
                          [\"4LZA\", \"5RU3\"]){cell {volume, angle_beta},
                          exptl {method}}}")
result <- search_graphql(graphql_json_query)
result
}
