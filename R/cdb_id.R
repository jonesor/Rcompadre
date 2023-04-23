#' Create integer identifiers for a COM(P)ADRE database corresponding to unique
#' combinations of a given set of columns
#'
#' Creates a vector of integer identifiers corresponding to the rows of a
#' CompadreDB object, based on unique combinations of the elements in a given
#' set of columns.
#'
#' @param cdb A CompadreDB object
#' @param columns Vector of column names from which unique combinations should
#'   be identified
#'
#' @return Vector of integer identifiers corresponding to the rows of
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
#' @examples
#' cdb_id(Compadre, columns = c("SpeciesAuthor", "MatrixTreatment"))
#'
#' @export cdb_id
cdb_id <- function(cdb, columns) {
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }

  col_check <- columns %in% names(cdb@data)
  if (!all(col_check)) {
    stop("cdb does not appear to have the following required columns: ",
      toString(columns[!col_check]),
      call. = FALSE
    )
  }

  cols <- cdb@data[, columns]
  col_collapse <- apply(cols, 1, paste, collapse = "")

  return(as.integer(as.factor(col_collapse)))
}
