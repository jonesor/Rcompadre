#' Flag potential issues in matrices of a COM(P)ADRE database
#' 
#' Adds columns to the data slot of a CompadreDB object that flag potential
#' problems in the matrix population models, such as matrices containing missing
#' values or all zeros, matrices that are non-invertable, or projection models
#' that are non-ergodic, reducible, or imprimitive. These columns can
#' subsequently be used to subset the database by logical argument.
#'
#' @param cdb A CompadreDB object
#' 
#' @return Returns \code{cdb} with extra columns appended to the data slot to
#'   indicate (TRUE/FALSE) whether there are potential problems with the
#'   matrices corresponding to a given row of the data, including whether each
#'   of the component matrices have missing values, whether matU contains only
#'   zeros, whether matU is non-invertable, or whether matA is ergodic,
#'   primitive, and irreducible.
#' 
#' @author Julia Jones <juliajones@@biology.sdu.dk>
#' @author Roberto Salguero-Goméz <rob.salguero@@zoo.ox.ac.uk>
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @examples
#' CompadreFlag <- cdb_flag(Compadre)
#'
#' @importFrom popdemo isErgodic isIrreducible isPrimitive
#' @importFrom methods new
#' @export cdb_flag
cdb_flag <- function(cdb) {
  
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }
  
  dat <- cdb@data
  
  matA <- matA(cdb)
  matU <- matU(cdb)
  matF <- matF(cdb)
  matC <- matC(cdb)
  
  dat$check_NA_A <- vapply(matA, function(x) any(is.na(x)), FALSE)
  dat$check_NA_U <- vapply(matU, function(x) any(is.na(x)), FALSE)
  dat$check_NA_F <- vapply(matF, function(x) any(is.na(x)), FALSE)
  dat$check_NA_C <- vapply(matC, function(x) any(is.na(x)), FALSE)
  dat$check_zero_U <- vapply(matU, function(x) all(x == 0 | is.na(x)), FALSE)
  
  
  dat$check_ergodic <- mapply(
    CheckMats,
    has_na = dat$check_NA_A,
    mat = matA,
    MoreArgs = list(fn = isErgodic)
  )
  
  dat$check_irreducible <- mapply(
    CheckMats,
    has_na = dat$check_NA_A,
    mat = matA,
    MoreArgs = list(fn = isIrreducible)
  )
  
  dat$check_primitive <- mapply(
    CheckMats,
    has_na = dat$check_NA_A,
    mat = matA,
    MoreArgs = list(fn = isPrimitive)
  )
  
  dat$check_singular_U <- mapply(
    CheckMats,
    has_na = dat$check_NA_A,
    mat = matU,
    MoreArgs = list(fn = CheckSingular)
  )
  
  new("CompadreDB",
      data = dat,
      version = cdb@version)
}



# utilities
CheckMats <- function(has_na, mat, fn) {
  fn <- match.fun(fn)
  ifelse(has_na, NA, fn(mat))
}

CheckSingular <- function(matU) {
  # try calculating fundamental matrix
  N <- try(solve(diag(nrow(matU)) - matU), silent = TRUE)
  
  # flag if singular
  ifelse(class(N) == 'try-error' && grepl('singular', N[1]),
         TRUE,
         FALSE)
}
