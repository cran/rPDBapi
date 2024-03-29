% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TextOperators.R
\name{ContainsWordsOperator}
\alias{ContainsWordsOperator}
\title{Create a Contains Words Search Operator}
\usage{
ContainsWordsOperator(attribute, value)
}
\arguments{
\item{attribute}{The attribute to be evaluated.}

\item{value}{The words to search for in the attribute.}
}
\value{
An object of class 'ContainsWordsOperator'.
}
\description{
Constructs a ContainsWordsOperator object for search operations that look for attributes containing certain words.
}
