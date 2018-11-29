#' Convert a COM(P)ADRE database object to a flat data frame
#'
#' Converts a CompadreDB object to a flat data frame, by converting each matrix
#' and associated matrixClass information to a string.
#'
#' @param db A CompadreDB object
#' @param onlyMatA A logical value (TRUE/FALSE) indicating whether ONLY the full
#'   projection matrix \code{matA} should be included in the flattened data
#'   frame
#' 
#' @return The \code{data.frame} from the data slot of \code{db}, but with
#'   additional columns appended for the matrix stage information and the
#'   matrices themselves, both in string format.
#' 
#' @details \code{DBToFlat} is preferred, but \code{convert2flat} is provided 
#' for legacy purposes.
#'
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' 
#' @seealso stringToMatrix
#' 
#' @examples
#' CompadreFlat <- DBToFlat(Compadre, onlyMatA = FALSE)
#' 
#' @export DBToFlat
DBToFlat <- function(db, onlyMatA = FALSE){
  
  if (!inherits(db, "CompadreDB")) {
    stop("db must be of class CompadreDB. See function asCompadreDB")
  }
  
  newdata <- CompadreData(db)[!(names(CompadreData(db)) %in% "mat")]
  
  newdata$MatrixClassAuthor <- sapply(MatrixClassAuthor(db), function(x) {
                                      paste(x, collapse = " | ") })
  
  newdata$matA <- vapply(matA(db), flattenMat, "")
  
  if(onlyMatA == FALSE) {
    newdata$matU <- vapply(matU(db), flattenMat, "")
    newdata$matF <- vapply(matF(db), flattenMat, "")
    newdata$matC <- vapply(matC(db), flattenMat, "")
  }
  return(newdata)
}

#' @rdname DBToFlat
convert2flat <- function(db, onlyMatA = FALSE){ DBToFlat(db, onlyMatA) }


# utility
flattenMat <- function(x) {
  paste0("[", paste(t(x), collapse=" "), "]")
}
