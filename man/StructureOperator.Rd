% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StructureOperator.R
\name{StructureOperator}
\alias{StructureOperator}
\title{Create a Structure Operator for Structure-Based Searches}
\usage{
StructureOperator(
  pdb_entry_id,
  assembly_id = 1,
  search_mode = "STRICT_SHAPE_MATCH"
)
}
\arguments{
\item{pdb_entry_id}{A string representing the PDB entry ID to search for.}

\item{assembly_id}{An integer representing the assembly ID, default is 1.}

\item{search_mode}{A string indicating the search mode, such as 'STRICT_SHAPE_MATCH', default is 'STRICT_SHAPE_MATCH'.}
}
\value{
An object of class 'StructureOperator' representing the structure search operator.
}
\description{
Constructs a StructureOperator object for use in structure-based searches within the RCSB PDB.
This operator allows specifying a PDB entry ID, assembly ID, and the search mode.
}
