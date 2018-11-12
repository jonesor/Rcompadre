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
  
  #new data.frame for data slot
  newdata <- data(db)

  # create row index
  newdata$index <- 1:NumberMatrices(db)
  
  # check matA, matU, matF, and matC for any values of NA
  newdata$matAcheckNA <- sapply(matA(db), function(x){ any(is.na(x)) })
  newdata$matUcheckNA <- sapply(matU(db), function(x){ any(is.na(x)) })
  newdata$matFcheckNA <- sapply(matF(db), function(x){ any(is.na(x)) })
  newdata$matCcheckNA <- sapply(matC(db), function(x){ any(is.na(x)) })
  
  # check whether any columns of matU have sums exceeding 1
  newdata$matUcolSums <- sapply(matU(db), 
    function(x){ any(colSums(x, na.rm = TRUE) > 1) })
  
  # check properties of matA using functions in popdemo
  # these checks require matA with no values of NA
  db_sub <- subsetDB(db, matAcheckNA %in% FALSE) # subset db to matA with no NAs
  newdata_sub <- data(db_sub)
  newdata_sub$checkPrimitive <- sapply(matA(db_sub), popdemo::isPrimitive)
  newdata_sub$checkIrreducible <- sapply(matA(db_sub), popdemo::isIrreducible)
  newdata_sub$checkErgodic <- sapply(matA(db_sub), popdemo::isErgodic)
  
  # merge checks into full db
  newdata_sub2 <- subset(newdata_sub, select = c('index',
                                                 'check_ergodic',
                                                 'check_primitive',
                                                 'check_irreducible'))
  newdata <- merge(newdata, newdata_sub2, by = 'index', all.x = T)
  newdata <- subset(newdata, select = -index)
  
  # return
  db@data <- newdata
  return(db)
}

#' @rdname cleanDB
cleanDatabase <- function(db) { cleanDB(db) }