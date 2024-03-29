#' Retrieve Specific Fields for Search Results from RCSB PDB
#'
#' This function performs a search in the Protein Data Bank (PDB) using a provided search term
#' and retrieves information for a specified field (e.g., citation) for each search result.
#' It relies on `query_search` and `get_info` functions for searching and retrieving detailed information.
#'
#' @param search_term A string specifying the term to search for in the PDB.
#' @param field A string indicating the specific field to retrieve for each search result.
#'   Default is "citation". Other options are 'audit_author', 'cell', 'diffrn', 'diffrn_detector',
#'   'diffrn_radiation', 'diffrn_source', 'entry', 'exptl', 'exptl_crystal', 'exptl_crystal_grow', 'pdbx_sgproject',
#'   'pdbx_audit_revision_details', 'pdbx_audit_revision_history', 'pdbx_database_related', 'pdbx_database_status',
#'   'rcsb_accession_info', 'rcsb_entry_container_identifiers', 'rcsb_entry_info', 'rcsb_primary_citation', 'refine',
#'   'refine_hist', 'refine_ls_restr', 'reflns', 'reflns_shell', 'software', 'struct', 'struct_keywords', 'symmetry',
#'   'rcsb_id'
#' @return A named list where each element's name is a PDB ID and its value is the information
#'   for the specified field from the corresponding search result.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @examples
#' \donttest{
#' find_results("crispr", field = "citation")
#'}
#' @export
find_results <- function(search_term, field = "citation") {

  # Retrieve search result IDs
  search_result_ids <- query_search(search_term)

  # Initialize a list to store results
  all_results <- list()

  # Iterate over each ID and fetch the required field
  for (i in 1:length(search_result_ids)) {
    pdb_info <- get_info(search_result_ids[i])
    if (field %in% names(pdb_info)) {
      all_results[[search_result_ids[i]]] <- pdb_info[[field]]
    }
  }

  return(all_results)
}


