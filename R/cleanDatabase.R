#' A function to create indicator vectors to flag problems in matrices
#' of the COMPADRE/COMADRE db
#' 
#' This function allows users to create input that subsequently can be used to 
#' subset the COMPADRE/COMADRE db by
#' logical argument.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#'
#'#' @export
#' @param db The COMPADRE or COMADRE db object.#' 
#' @return Returns the db, with an additional set of metadata to indicate
#' in T/F vector forms whether or not the different matrices have issues with the data
#' plus whether or not the AMat is irreducible, ergodic, and primitive.
#' @note %% ~~further notes~~
#' @author Julia Jones <juliajones@@biology.sdu.dk>
#' 
#' Roberto Salguero-Goméz
#' 
#' Danny Buss
#' 
#' Patrick Barks
#' 
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% references
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' 
#' 
#' 
cleanDatabase <- function(db) {
  
  # create row index
  db$metadata$index <- 1:nrow(db$metadata)
  
  # check matA, matU, matF, and matC for any values of NA
  db$metadata$check_NA_A <- sapply(db$mat, function(x) any(is.na(x$matA)))
  db$metadata$check_NA_U <- sapply(db$mat, function(x) any(is.na(x$matU)))
  db$metadata$check_NA_F <- sapply(db$mat, function(x) any(is.na(x$matF)))
  db$metadata$check_NA_C <- sapply(db$mat, function(x) any(is.na(x$matC)))
  
  # check whether any columns of matU have sums exceeding 1
  checkColsums <- function(x) any(colSums(x$matU, na.rm = T) > 1)
  db$metadata$check_colsums_U <- sapply(db$mat, checkColsums)
  
  # check properties of matA using functions in popdemo
  # these checks require matA with no values of NA
  db_sub <- subsetDB(db, check_NA_A == F) # subset db to matA with no NAs
  
  checkErgodic <- function(x) popdemo::isErgodic(x$matA)
  checkPrimitive <- function(x) popdemo::isPrimitive(x$matA)
  checkIrreducible <- function(x) popdemo::isIrreducible(x$matA)
  
  db_sub$metadata$check_ergodic <- sapply(db_sub$mat, checkErgodic)
  db_sub$metadata$check_primitive <- sapply(db_sub$mat, checkPrimitive)
  db_sub$metadata$check_irreducible <- sapply(db_sub$mat, checkIrreducible)
  
  # merge checks into full db
  db_sub$metadata <- subset(db_sub$metadata,select = c('index',
                                                       'check_ergodic',
                                                       'check_primitive',
                                                       'check_irreducible'))
  db$metadata <- merge(db$metadata, db_sub$metadata, by = 'index', all.x = T)
  db$metadata <- subset(db$metadata, select = -index)
  
  # return
  return(db)
}