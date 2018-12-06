#' Flag potential issues in matrices of a COM(P)ADRE database
#' 
#' @description 
#' Adds columns to the data slot of a CompadreDB object that flag potential
#' problems in the matrix population models, such as matrices containing missing
#' values or all zeros, matrices that are non-invertable, projection models in
#' which the \strong{U}/\strong{F}/\strong{C} components do not sum to
#' \strong{A}, or projection models that are non-ergodic, reducible, or
#' imprimitive. These columns can subsequently be used to subset the database by
#' logical argument.
#' 
#' @param cdb A CompadreDB object
#' @param check_NA_A check for missing values in matA?
#' @param check_NA_U check for missing values in matU?
#' @param check_NA_F check for missing values in matF?
#' @param check_NA_C check for missing values in matC?
#' @param check_zero_U check whether matU all zeros (including NA)?
#' @param check_singular_U check whether matU singular (i.e. non-invertable)?
#' @param check_component_sum do matU, matF, and matC components sum to matA
#'   (see \emph{Details})?
#' @param check_ergodic check whether matA ergodic (see
#'   \code{\link[popdemo]{isErgodic}})?
#' @param check_irreducible check whether matA irreducible (see
#'   \code{\link[popdemo]{isIrreducible}})?
#' @param check_primitive check whether matA primitive (see
#'   \code{\link[popdemo]{isPrimitive}})?
#' 
#' @return Returns \code{cdb} with extra columns appended to the data slot
#'   (columns have the same names as the corresponding \code{check_} arguments)
#'   to indicate (TRUE/FALSE) whether there are potential problems with the
#'   matrices corresponding to a given row of the data.
#' 
#' @details 
#' For the flag \code{check_component_sum}, a value of \code{NA} will be
#' returned if the matrix sum of matU, matF, and matC consists only of zeros
#' and/or \code{NA}, indicating that the matrix has not been split.
#' 
#' @author Julia Jones <juliajones@@biology.sdu.dk>
#' @author Roberto Salguero-Goméz <rob.salguero@@zoo.ox.ac.uk>
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @examples
#' CompadreFlag <- cdb_flag(Compadre)
#' 
#' # exclude checks for missing values
#' CompadreFlag <- cdb_flag(Compadre,
#'                          check_NA_A = FALSE,
#'                          check_NA_U = FALSE,
#'                          check_NA_F = FALSE,
#'                          check_NA_C = FALSE)
#'
#' @importFrom popdemo isErgodic isIrreducible isPrimitive
#' @importFrom methods new
#' @export cdb_flag
cdb_flag <- function(cdb,
                     check_NA_A = TRUE,
                     check_NA_U = TRUE,
                     check_NA_F = TRUE,
                     check_NA_C = TRUE,
                     check_zero_U = TRUE,
                     check_singular_U = TRUE,
                     check_component_sum = TRUE,
                     check_ergodic = TRUE,
                     check_irreducible = TRUE,
                     check_primitive = TRUE) {
  
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }
  
  dat <- cdb@data
  
  matA <- matA(cdb)
  matU <- matU(cdb)
  matF <- matF(cdb)
  matC <- matC(cdb)
  
  # calculate outside conditionals because may be required later
  vec_NA_A <- vapply(matA, function(x) any(is.na(x)), FALSE)
  vec_NA_U <- vapply(matU, function(x) any(is.na(x)), FALSE)
  vec_NA_F <- vapply(matF, function(x) any(is.na(x)), FALSE)
  vec_NA_C <- vapply(matC, function(x) any(is.na(x)), FALSE)
  
  if (check_NA_A) {
    dat$check_NA_A <- vec_NA_A
  }
  if (check_NA_U) {
    dat$check_NA_U <- vec_NA_U
  }
  if (check_NA_F) {
    dat$check_NA_F <- vec_NA_F
  }
  if (check_NA_C) {
    dat$check_NA_C <- vec_NA_C
  }
  if (check_zero_U) {
    dat$check_zero_U <- vapply(matU, function(x) all(x == 0 | is.na(x)), FALSE)
  }
  
  if (check_singular_U) {
    dat$check_singular_U <- mapply(
      CheckMats,
      has_na = vec_NA_U,
      mat = matU,
      MoreArgs = list(fn = CheckSingular)
    )
  }
  
  if (check_component_sum) {
    dat$check_component_sum <- mapply(ComponentSum, matA, matU, matF, matC)
  }
  
  if (check_ergodic) {
    dat$check_ergodic <- mapply(
      CheckMats,
      has_na = vec_NA_A,
      mat = matA,
      MoreArgs = list(fn = isErgodic)
    )
  }
  
  if (check_irreducible) {
    dat$check_irreducible <- mapply(
      CheckMats,
      has_na = vec_NA_A,
      mat = matA,
      MoreArgs = list(fn = isIrreducible)
    )
  }
  
  if (check_primitive) {
    dat$check_primitive <- mapply(
      CheckMats,
      has_na = vec_NA_A,
      mat = matA,
      MoreArgs = list(fn = isPrimitive)
    )
  }
  
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

ComponentSum <- function(mA, mU, mF, mC) {
  mat_dim <- nrow(mA)
  
  if (all(is.na(mU))) mU <- matrix(0, mat_dim, mat_dim)
  if (all(is.na(mF))) mF <- matrix(0, mat_dim, mat_dim)
  if (all(is.na(mC))) mC <- matrix(0, mat_dim, mat_dim)
  
  mat_sum <- mU + mF + mC
  
  if (all(mat_sum == 0 | is.na(mat_sum))) {
    out <- NA
  } else {
    val_check <- mapply(function(x, y) isTRUE(all.equal(x, y)),
                        x = c(mat_sum), y = c(mA))
    
    out <- ifelse(all(val_check), TRUE, FALSE)
  }
  
  return(out)
}
