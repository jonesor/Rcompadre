#' Convert a COM(P)ADRE database to a tidy data frame
#'
#' Converts a CompadreDB object to a tidy data frame by extracting the data
#' slot, and splitting the \code{mat} column into separate list-columns for each
#' component (matrices \code{matA}, \code{matU}, \code{matF}, \code{matC}, and
#' vectors \code{MatrixClassAuthor}, and \code{MatrixClassOrganized}).
#'
#' @param cdb A CompadreDB object
#' 
#' @return A tibble-style data frame based on the data slot of \code{cdb}, but
#'   with the list-column \code{mat} split into 6 separate list-columns (for
#'   matrices \code{matA}, \code{matU}, \code{matF}, \code{matC}, and vectors
#'   \code{MatrixClassAuthor}, and \code{MatrixClassOrganized}).
#'
#' @author Patrick M. Barks <patrick.barks@@gmail.com>
#' 
#' @examples
#' CompadreTidy <- cdb_tidy(Compadre)
#' 
#' @export cdb_tidy
cdb_tidy <- function(cdb) {
  
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }
  
  dat <- cdb@data
  dat$matA <- matA(cdb)
  dat$matU <- matU(cdb)
  dat$matF <- matF(cdb)
  dat$matC <- matC(cdb)
  dat$MatrixClassAuthor <- MatrixClassAuthor(cdb)
  dat$MatrixClassOrganized <- MatrixClassOrganized(cdb)
  dat$mat <- NULL
  
  return(dat)
}
