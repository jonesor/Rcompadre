#' Extract metadata from a COM(P)ADRE database
#'
#' Extract a tibble with only metadata information from a CompadreDB object, by
#' dropping the matrix column "mat".
#'
#' @param cdb A CompadreDB object
#'
#' @return Tibble with all metadata columns of \code{cdb}
#'
#' @author Gesa Römer <gesa.roemer@@gmail.com>
#'
#' @family data acquisition
#'
#' @details Transforms the large CompadreDB object into a tibble and drops the
#' matrix column ("mat").
#'
#' @examples
#' Compadre_metadata <- cdb_metadata(Compadre)
#'
#' @export cdb_metadata
cdb_metadata <- function(cdb) {
  cdb <- as_tibble(cdb)
  i <- which(names(cdb) == "mat")
  if (length(i) == 0) stop("argument cdb does not contain a column 'mat'")
  cdb <- cdb[, -i]
  return(cdb)
}
