#' Convert a list-structured COM(P)ADRE database object to a flat data frame
#'
#' This function converts a list-structured COM(P)ADRE database object to a flat
#' data frame, by converting each matrix and associated matrixClass information
#' to a string.
#'
#' @param db A COM(P)ADRE database object. Database versions <=4.0.1 will be coerced
#'  from class 'list.
#' @param onlyMatA A logical value (TRUE/FALSE) indicating whether ONLY the full
#'   projection matrix \code{matA} should be included in the flattened data
#'   frame
#' @return The \code{data.frame} from the metadata slot of \code{db}, but with
#'   additional columns appended for the matrix stage information and the
#'   matrices themselves, both in string format.
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @seealso stringtomatrix
#' @examples
#' \dontrun{
#' compadreFlat <- convert2flat(compadre, onlyMatA = FALSE)
#' }
#' @export convert2flat
convert2flat <- function(db, onlyMatA = FALSE){
  # convert legacy versions of COM(P)ADRE from class 'list' to 'CompadreData'
  if (class(db) == "list"){
    if (as.numeric(gsub("\\.", "", sub("(\\s.*$)", "", db$version$Version))) <= 401){
      db <- as(db, "CompadreData")
    }
  }
  
  db@metadata$Amatrix <- NULL
  for (i in 1:nrow(db@metadata)){
    db@metadata$classnames[i] <- paste(db@mat[[i]]@matrixClass$MatrixClassAuthor,
                                       collapse = " | ")
    db@metadata$matrixA[i] <- paste("[", paste(t(db@mat[[i]]@matA), collapse=" "), "]",
                                    sep = "")
  }
  
  if(onlyMatA == FALSE) {
    for (i in 1:nrow(db@metadata)){
      db@metadata$matrixU[i] <- paste("[", paste(t(db@mat[[i]]@matU), collapse=" "),
                                      "]", sep = "")
      db@metadata$matrixF[i] <- paste("[", paste(t(db@mat[[i]]@matF), collapse=" "),
                                      "]", sep = "")
      db@metadata$matrixC[i] <- paste("[", paste(t(db@mat[[i]]@matC), collapse=" "),
                                      "]", sep = "")
    }
  }
  
  return(db@metadata)
}
