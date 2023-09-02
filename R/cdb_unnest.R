#' Unnest a COM(P)ADRE database by spreading the components of CompadreMat into
#' separate list-columns
#'
#' @description
#' Unnests a CompadreDB object by spreading the components of CompadreMat into
#' separate list-columns. Components that may be extracted include:
#' \itemize{
#'   \item \code{matA} (matrix)
#'   \item \code{matU} (matrix)
#'   \item \code{matF} (matrix)
#'   \item \code{matC} (matrix)
#'   \item \code{MatrixClassAuthor} (character vector)
#'   \item \code{MatrixClassOrganized} (character vector)
#'   \item \code{MatrixClassNumber} (integer vector)
#' }
#'
#' @param cdb A CompadreDB object
#' @param components Character vector specifying which components to extract.
#'
#'   Defaults to all, i.e. \code{c("matA", "matU", "matF", "matC",
#'   "MatrixClassAuthor", "MatrixClassOrganized", "MatrixClassNumber")}
#'
#' @return \code{cdb} with additional list-columns for each element of argument
#'   \code{components}
#'
#' @author Patrick M. Barks <patrick.barks@@gmail.com>
#'
#' @family data management
#'
#' @examples
#' # unnest all components
#' CompadreUnnest <- cdb_unnest(Compadre)
#'
#' # unnest select components (matU and MatrixClassAuthor)
#' CompadreUnnest <- cdb_unnest(Compadre, c("matU", "MatrixClassAuthor"))
#'
#' @export cdb_unnest
cdb_unnest <- function(cdb, components = c(
                         "matA",
                         "matU",
                         "matF",
                         "matC",
                         "MatrixClassAuthor",
                         "MatrixClassOrganized",
                         "MatrixClassNumber"
                       )) {
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }

  comp_allow <- c(
    "matA",
    "matU",
    "matF",
    "matC",
    "MatrixClassAuthor",
    "MatrixClassOrganized",
    "MatrixClassNumber"
  )

  comp_check <- components %in% comp_allow

  if (!any(comp_check)) {
    stop("The following elements of argument 'components' are not valid: ",
      toString(components[!comp_check]),
      call. = FALSE
    )
  }

  if ("matA" %in% components) {
    cdb$matA <- matA(cdb)
  }
  if ("matU" %in% components) {
    cdb$matU <- matU(cdb)
  }
  if ("matF" %in% components) {
    cdb$matF <- matF(cdb)
  }
  if ("matC" %in% components) {
    cdb$matC <- matC(cdb)
  }
  if ("MatrixClassAuthor" %in% components) {
    cdb$MatrixClassAuthor <- MatrixClassAuthor(cdb)
  }
  if ("MatrixClassOrganized" %in% components) {
    cdb$MatrixClassOrganized <- MatrixClassOrganized(cdb)
  }
  if ("MatrixClassNumber" %in% components) {
    cdb$MatrixClassNumber <- MatrixClassNumber(cdb)
  }

  return(cdb)
}
