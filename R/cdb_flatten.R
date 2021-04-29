#' Convert a COM(P)ADRE database to a flat data frame with matrices and vectors
#' stored in string representation
#'
#' Converts a CompadreDB object to a flat data frame by extracting the data
#' slot, and splitting the \code{mat} column into separate columns for each
#' component (matrices \code{matA}, \code{matU}, \code{matF}, \code{matC}, and
#' vectors \code{MatrixClassAuthor}, and \code{MatrixClassOrganized}). The
#' component matrices and vectors within the six new columns are stored in
#' string format so that the database can be written to a flat file format such
#' as csv (see \link{string_representation}).
#'
#' @param cdb A CompadreDB object
#' 
#' @return A data frame based on the data slot of \code{cdb}, but with the
#'   column \code{mat} replaced by six separate columns (for matrices
#'   \code{matA}, \code{matU}, \code{matF}, \code{matC}, and vectors
#'   \code{MatrixClassAuthor}, and \code{MatrixClassOrganized}), whose elements
#'   are matrices or vectors in string representation.
#'
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @author Patrick M. Barks <patrick.barks@@gmail.com>
#' 
#' @family data management
#' 
#' @seealso \link{cdb_unflatten} \link{string_representation}
#' 
#' @examples
#' CompadreFlat <- cdb_flatten(Compadre)
#' 
#' @export cdb_flatten
cdb_flatten <- function(cdb) {
  
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }
  
  cdb <- cdb_unnest(cdb)
  db <- cdb@data
  db$mat <- NULL
  
  db$matA <- vapply(db$matA, mat_to_string, "")
  db$matU <- vapply(db$matU, mat_to_string, "")
  db$matF <- vapply(db$matF, mat_to_string, "")
  db$matC <- vapply(db$matC, mat_to_string, "")
  db$MatrixClassAuthor <- vapply(db$MatrixClassAuthor, vec_to_string, "")
  db$MatrixClassOrganized <- vapply(db$MatrixClassOrganized, vec_to_string, "")
  
  return(db)
}
