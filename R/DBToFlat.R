#' Convert a list-structured COM(P)ADRE database object to a flat data frame
#'
#' This function converts a list-structured COM(P)ADRE database object to a flat
#' data frame, by converting each matrix and associated matrixClass information
#' to a string.
#'
#' @param db A COM(P)ADRE database object. Databases will be will be coerced
#'  from the old 'list' format where appropriate (compadre_v4.0.1 and below; 
#' comadre_v2.0.1 and below).
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
#' 
DBToFlat <- function(db, onlyMatA = FALSE){
  if (class(db) == "list"){
    if( "Animalia" %in% db$metadata$Kingdom ) vlim <- 201
    if( "Plantae" %in% db$metadata$Kingdom ) vlim <- 401
    if (as.numeric(gsub("\\.", "", sub("(\\s.*$)", "", db$version$Version))) <= vlim){
      db <- methods::as(db, "CompadreDB")
    }
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