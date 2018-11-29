#' Convert a COM(P)ADRE database object to a flat data frame
#'
#' Converts a CompadreDB object to a flat data frame, by converting each matrix
#' and associated matrixClass information to a string.
#'
#' @param cdb A CompadreDB object
#' @param onlyMatA A logical value (TRUE/FALSE) indicating whether ONLY the full
#'   projection matrix \code{matA} should be included in the flattened data
#'   frame
#' 
#' @return The \code{data.frame} from the data slot of \code{cdb}, but with
#'   additional columns appended for the matrix stage information and the
#'   matrices themselves, both in string format.
#'
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' 
#' @seealso string_to_mat
#' 
#' @examples
#' CompadreFlat <- cdb_flatten(Compadre, onlyMatA = FALSE)
#' 
#' @export cdb_flatten
cdb_flatten <- function(cdb, onlyMatA = FALSE){
  
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }
  
  newdata <- CompadreData(cdb)[!(names(CompadreData(cdb)) %in% "mat")]
  
  newdata$MatrixClassAuthor <- sapply(MatrixClassAuthor(cdb), function(x) {
                                      paste(x, collapse = " | ") })
  
  newdata$matA <- vapply(matA(cdb), flattenMat, "")
  
  if(onlyMatA == FALSE) {
    newdata$matU <- vapply(matU(cdb), flattenMat, "")
    newdata$matF <- vapply(matF(cdb), flattenMat, "")
    newdata$matC <- vapply(matC(cdb), flattenMat, "")
  }
  return(newdata)
}


# utility
flattenMat <- function(x) {
  paste0("[", paste(t(x), collapse=" "), "]")
}
