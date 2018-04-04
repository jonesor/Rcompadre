#' Flag potential issues in matrices of a COM(P)ADRE database.
#' 
#' This function adds columns to the metatadata slot of a COM(P)ADRE database
#' object that flag potential problems in the matrices, such as when matrices
#' contain missing values. These columns can subsequently can be used to subset
#' the COM(P)ADRE database by logical argument.
#'
#' @param db A COM(P)ADRE database object. Databases will be will be coerced
#'  from the old 'list' format where appropriate (compadre_v4.0.1 and below; 
#' comadre_v2.0.1 and below).
#' 
#' @return Returns db with extra columns appended to the metadata to indicate
#'   (TRUE/FALSE) whether there are potential problems with the matrices
#'   corresponding to a given row of the metadata, including whether matA is
#'   ergodic, primitive, and irreducible.
#' 
#' @details \code{cleanDB} is preferred, but \code{cleanDatabase} is provided 
#' for legacy purposes.
#' 
#' @author Julia Jones <juliajones@@biology.sdu.dk>
#' @author Roberto Salguero-Goméz <rob.salguero@@zoo.ox.ac.uk>
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @keywords utilities
#' 
#' @examples
#' \dontrun{
#' compadre_clean <- cleanDB(compadre)
#' }
#'
#' @importFrom popdemo is.matrix_ergodic is.matrix_primitive is.matrix_irreducible
#' @importFrom methods as
#' 
#' @export cleanDB
#' 
cleanDB <- function(db) {
  
  db <- convertLegacyDB(db)
  
  # create row index
  db@metadata$index <- 1:nrow(db@metadata)
  
  # check matA, matU, matF, and matC for any values of NA
  db@metadata$check_NA_A <- sapply(db@mat, function(x) any(is.na(x@matA)))
  db@metadata$check_NA_U <- sapply(db@mat, function(x) any(is.na(x@matU)))
  db@metadata$check_NA_F <- sapply(db@mat, function(x) any(is.na(x@matF)))
  db@metadata$check_NA_C <- sapply(db@mat, function(x) any(is.na(x@matC)))
  
  # check whether any columns of matU have sums exceeding 1
  checkColsums <- function(x) any(colSums(x@matU, na.rm = TRUE) > 1)
  db@metadata$check_colsums_U <- sapply(db@mat, checkColsums)
  
  # check properties of matA using functions in popdemo
  # these checks require matA with no values of NA
  db_sub <- subsetDB(db, check_NA_A == F) # subset db to matA with no NAs
  
  checkErgodic <- function(x) popdemo::is.matrix_ergodic(x@matA)
  checkPrimitive <- function(x) popdemo::is.matrix_primitive(x@matA)
  checkIrreducible <- function(x) popdemo::is.matrix_irreducible(x@matA)
  
  db_sub@metadata$check_ergodic <- sapply(db_sub@mat, checkErgodic)
  db_sub@metadata$check_primitive <- sapply(db_sub@mat, checkPrimitive)
  db_sub@metadata$check_irreducible <- sapply(db_sub@mat, checkIrreducible)
  
  # merge checks into full db
  db_sub@metadata <- subset(db_sub@metadata, select = c('index',
                                                        'check_ergodic',
                                                        'check_primitive',
                                                        'check_irreducible'))
  db@metadata <- merge(db@metadata, db_sub@metadata, by = 'index', all.x = T)
  db@metadata <- subset(db@metadata, select = -index)
  
  # return
  return(db)
}

#' @rdname cleanDB
cleanDatabase <- function(db) { cleanDB(db) }