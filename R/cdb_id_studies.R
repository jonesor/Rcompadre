#' Create a vector of unique study identifiers from a COM(P)ADRE database
#'
#' Creates a vector of integer study identifiers corresponding to the rows of a
#' CompadreDB object, based on unique combinations of the columns 'Authors',
#' 'Journal', 'YearPublication', and 'DOI_ISBN' (or optionally, a different set
#' of columns supplied by the user).
#'
#' @param cdb A CompadreDB object
#' @param columns Vector of column names from which unique combinations should
#'   be identified. Defaults to \code{c("Authors", "Journal", "YearPublication",
#'   "DOI_ISBN")}.
#' 
#' @return Vector of integer study identifiers corresponding to the rows of
#'   \code{cdb}, based on unique combinations of the elements in \code{columns}.
#' 
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @family data management
#' 
#' @details
#' Identifiers are assigned by pasting together the relevant columns, assigning
#' factor levels based on alphabetical order, and then converting the factor
#' levels to integers.
#' 
#' @seealso \link{cdb_id}
#' 
#' @examples
#' Compadre$StudyID <- cdb_id_studies(Compadre)
#' 
#' @export cdb_id_studies
cdb_id_studies <- function(cdb, columns = c("Authors", "Journal",
                                            "YearPublication", "DOI_ISBN")) {

  cdb_id(cdb, columns)
}

