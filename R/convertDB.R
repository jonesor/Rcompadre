#' utility to convert older versions of database to
#' modern class
#' 
#' @param db The \code{Com(p)adre} data base to convert
#' 
#' @return A \code{CompadreDB} object
#' 
#' @importFrom methods as
#' 
#' @export

convertLegacyDB <- function(db) { 
  if (!inherits(db, 'CompadreDB')){
    requiredFields <- c('metadata', 'mat', 'matrixClass')
    
    if(all(requiredFields %in% names(db))){
      db <- methods::as(db, "CompadreDB")
    } else {
      stop("'CompadreDB' objects require at least the following fields:\n",
           "1. 'metadata'\n",
           "2. 'mat'\n",
           "3. 'matrixClass'\n",
           "Please download a newer version from www.compadre-db.org or query ",
           "the database API directly from R.")
    }
  }  
  return(db)
} 