#' Reconstitute a flattened COM(P)ADRE database (created by cdb_flatten) as a
#' CompadreDB object
#'
#' Converts a flattened COM(P)ADRE database (created by \link{cdb_flatten}) back
#' to the CompadreDB class
#'
#' @param db A data frame created with \link{cdb_flatten}, with columns for
#'   matrices \code{matA}, \code{matU}, \code{matF}, \code{matC}, and vectors
#'   \code{MatrixClassAuthor}, and \code{MatrixClassOrganized} in string
#'   representation.
#' 
#' @return A CompadreDB object. Because version details are lost when the
#'   database is flattened, the \code{Version} and \code{DateCreated} elements
#'   of the returned CompadreDB object will be \code{NA}.
#'
#' @author Patrick M. Barks <patrick.barks@@gmail.com>
#' 
#' @family data management
#' 
#' @seealso \link{cdb_flatten} \link{string_representation}
#' 
#' @examples
#' CompadreFlat <- cdb_flatten(Compadre)    # flatten
#' Compadre2 <- cdb_unflatten(CompadreFlat) # reconstitute
#' 
#' @importFrom tibble as_tibble add_column
#' @importFrom methods new
#' @export cdb_unflatten
cdb_unflatten <- function(db) {
  
  if (!"data.frame" %in% class(db)) {
    stop("db must have class data.frame")
  }
  
  matA <- lapply(db$matA, string_to_mat)
  matU <- lapply(db$matU, string_to_mat)
  matF <- lapply(db$matF, string_to_mat)
  matC <- lapply(db$matC, string_to_mat)
  MatrixClassOrganized <- lapply(db$MatrixClassOrganized, string_to_vec)
  MatrixClassAuthor <- lapply(db$MatrixClassAuthor, string_to_vec)
  
  # create mat list-column
  mat <- lapply(seq_along(matA), function(i) {
    new("CompadreMat",
        matA = matA[[i]],
        matU = matU[[i]],
        matF = matF[[i]],
        matC = matC[[i]],
        matrixClass = AsMatrixClass(MatrixClassOrganized[[i]],
                                    MatrixClassAuthor[[i]]))
  })
  
  # remove mat-related columns no longer needed
  db$matA <- NULL
  db$matU <- NULL
  db$matF <- NULL
  db$matC <- NULL
  db$MatrixClassOrganized <- NULL
  db$MatrixClassAuthor <- NULL
  
  # add matrices to metadata as a list column
  dat <- as_tibble(db)
  dat <- add_column(dat, mat = mat, .before = 1)
  
  # create list for version slot
  version <- list(
    Version = NA,
    DateCreate = NA,
    Agreement = "http://www.compadre-db.org/Page/UserAgreement"
  )
  
  # create CompadreDB object
  new("CompadreDB",
      data = dat,
      version = version)
}



# utility
AsMatrixClass <- function(MatrixClassOrganized, MatrixClassAuthor) {
  out <- data.frame(MatrixClassOrganized, MatrixClassAuthor)
  out$MatrixClassNumber <- seq_len(nrow(out))
  return(as.data.frame(out))
}
