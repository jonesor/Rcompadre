#' Convert a list-structured COM(P)ADRE database object to a flat data frame
#'
#' This function converts a list-structured COM(P)ADRE database object to a flat
#' data frame, by converting each matrix and associated matrixClass information
#' to a string.
#'
#' @param db A COM(P)ADRE database object.
#' @param onlyMatA A logical value (TRUE/FALSE) indicating whether ONLY the full
#'   projection matrix \code{matA} should be included in the flattened data
#'   frame
#' 
#' @return The \code{data.frame} from the metadata slot of \code{db}, but with
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
#' \dontrun{
#' compadreFlat <- DBToFlat(compadre, onlyMatA = FALSE)
#' }
#' 
#' @importFrom methods as
#' @export DBToFlat
DBToFlat <- function(db, onlyMatA = FALSE){
  
  if (!inherits(db, "CompadreDB")) {
    stop("db must be of class CompadreDB. See function convertLegacyDB")
  }
  
  newdata <- CompadreData(db)[!(names(CompadreData(db) %in% "mat"))]
  newdata$MatrixClassAuthor <- sapply(MatrixClassAuthor(db), function(x) {
                                      paste(x, collapse = " | ") })
  newdata$matA <- sapply(matA(db), function(x){
                         paste("[", paste(t(x), collapse=" "), "]", sep = "") })
  if(onlyMatA == FALSE) {
    newdata$matU <- sapply(matU(db), function(x){
                          paste("[", paste(t(x), collapse=" "), "]", sep = "") })
    newdata$matF <- sapply(matF(db), function(x){
                          paste("[", paste(t(x), collapse=" "), "]", sep = "") })
    newdata$matC <- sapply(matC(db), function(x){
                          paste("[", paste(t(x), collapse=" "), "]", sep = "") })
  }
  return(newdata)
}

#' @rdname DBToFlat
convert2flat <- function(db, onlyMatA = FALSE){ DBToFlat(db, onlyMatA) }
