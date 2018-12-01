#' Extract a vector of unique study identifiers from a COM(P)ADRE database
#'
#' Extracts a vector of integer study identifiers corresponding to the rows of a
#' CompadreDB object, based on unique combinations of the columns 'Authors',
#' 'Journal', 'YearPublication', and 'DOI.ISBN' (or optionally, a different set
#' of columns supplied by the user).
#'
#' @param cdb A CompadreDB object
#' @param columns A vector of column names for which unique combinations should
#'   be identified. Defaults to \code{c("Authors", "Journal", "YearPublication",
#'   "DOI.ISBN")}.
#' 
#' @return Vector of integer study identifiers corresponding to the rows of
#'   \code{cdb}.
#' 
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @examples
#' Compadre$StudyID <- cdb_vec_studies(Compadre)
#' 
#' @export cdb_vec_studies
cdb_vec_studies <- function(cdb, columns = c("Authors", "Journal",
                                             "YearPublication", "DOI.ISBN")) {

  if (!inherits(cdb, "CompadreDB")) {
    stop("db must be of class CompadreDB. See function as_cdb")
  }
  
  col_check <- columns %in% names(cdb@data)
  if (!all(col_check)) {
    stop("cdb does not appear to have the following required columns: ",
         paste(columns[!col_check], collapse = ", ") , call. = FALSE)
  }
  
  cols <- cdb@data[,columns]
  col_collapse <- apply(cols, 1, paste0, collapse = "")
  
  return(as.integer(as.factor(col_collapse)))
}

