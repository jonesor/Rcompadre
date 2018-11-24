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
#' @importFrom popdemo isErgodic isIrreducible isPrimitive
#' @importFrom methods new
#' @export cleanDB
cleanDB <- function(db) {
  
  if (!inherits(db, "CompadreDB")) {
    stop("db must be of class CompadreDB. See function convertLegacyDB")
  }
  
  dat <- CompadreData(db)
  
  dat$check_NA_A <- vapply(db$mat, function(x) any(is.na(x@matA)), logical(1))
  dat$check_NA_U <- vapply(db$mat, function(x) any(is.na(x@matU)), logical(1))
  dat$check_NA_F <- vapply(db$mat, function(x) any(is.na(x@matF)), logical(1))
  dat$check_NA_C <- vapply(db$mat, function(x) any(is.na(x@matC)), logical(1))
  
  dat$check_ergodic <- mapply(
    CheckMats,
    has_na = dat$check_NA_A,
    mat = matA(db),
    MoreArgs = list(fn = popdemo::isErgodic)
  )
  
  dat$check_irreducible <- mapply(
    CheckMats,
    has_na = dat$check_NA_A,
    mat = matA(db),
    MoreArgs = list(fn = popdemo::isIrreducible)
  )
  
  dat$check_primitive <- mapply(
    CheckMats,
    has_na = dat$check_NA_A,
    mat = matA(db),
    MoreArgs = list(fn = popdemo::isPrimitive)
  )
  
  new("CompadreDB",
      CompadreData = dat,
      VersionData = VersionData(db))
}



# utility
CheckMats <- function(has_na, mat, fn) {
  fn <- match.fun(fn)
  ifelse(has_na, NA, fn(mat))
}

