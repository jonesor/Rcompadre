#' Flag potential issues in matrices of a COM(P)ADRE database
#' 
#' @description 
#' Adds columns to the data slot of a CompadreDB object that flag potential
#' problems in the matrix population models. These columns can subsequently be
#' used to subset the database by logical argument.
#' 
#' Optional checks include:
#' \itemize{
#'   \item \code{check_NA_A}: missing values in matA?
#'   \item \code{check_NA_U}: missing values in matU?
#'   \item \code{check_NA_F}: missing values in matF?
#'   \item \code{check_NA_C}: missing values in matC?
#'   \item \code{check_zero_U}: matU all zeros (including NA)?
#'   \item \code{check_singular_U}: matU singular (i.e. non-invertable)?
#'   \item \code{check_component_sum}: do matU/matF/matC components sum to matA
#'     (see \emph{Details})?
#'   \item \code{check_ergodic}: is matA ergodic (see
#'     \code{\link[popdemo]{isErgodic}})?
#'   \item \code{check_irreducible}: is matA irreducible (see
#'     \code{\link[popdemo]{isIrreducible}})?
#'   \item \code{check_primitive}: is matA primitive (see
#'     \code{\link[popdemo]{isPrimitive}})?
#'   \item \code{check_surv_gte_1}: does matU contains values that are equal to or greater than 1?
#' }
#' 
#' @param cdb A CompadreDB object
#' @param checks Character vector specifying which checks to run.
#' 
#'   Defaults to all, i.e. \code{c("check_NA_A", "check_NA_U", "check_NA_F",
#'   "check_NA_C", "check_zero_U", "check_singular_U", "check_component_sum",
#'   "check_ergodic", "check_irreducible", "check_primitive", "check_surv_gte_1")}
#' 
#' @return Returns \code{cdb} with extra columns appended to the data slot
#'   (columns have the same names as the corresponding elements of
#'   \code{checks}) to indicate (TRUE/FALSE) whether there are potential
#'   problems with the matrices corresponding to a given row of the data.
#' 
#' @details 
#' For the flag \code{check_component_sum}, a value of \code{NA} will be
#' returned if the matrix sum of matU, matF, and matC consists only of zeros
#' and/or \code{NA}, indicating that the matrix has not been split.
#' 
#' @author Owen Jones <jones@@biology.sdu.dk>
#' @author Julia Jones <juliajones@@biology.sdu.dk>
#' @author Roberto Salguero-Gomez <rob.salguero@@zoo.ox.ac.uk>
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @family data checking
#' 
#' @examples
#' CompadreFlag <- cdb_flag(Compadre)
#' 
#' # only check whether matA has missing values, and whether matA is ergodic
#' CompadreFlag <- cdb_flag(Compadre, checks = c("check_NA_A", "check_ergodic"))
#' 
#' @importFrom popdemo isErgodic isIrreducible isPrimitive
#' @importFrom methods new
#' @export cdb_flag
cdb_flag <- function(cdb, checks = c("check_NA_A",
                                     "check_NA_U",
                                     "check_NA_F",
                                     "check_NA_C",
                                     "check_zero_U",
                                     "check_singular_U",
                                     "check_component_sum",
                                     "check_ergodic",
                                     "check_irreducible",
                                     "check_primitive",
                                     "check_surv_gte_1")) {
  
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }
  
  checks_allow <- c("check_NA_A",
                    "check_NA_U",
                    "check_NA_F",
                    "check_NA_C",
                    "check_zero_U",
                    "check_singular_U",
                    "check_component_sum",
                    "check_ergodic",
                    "check_irreducible",
                    "check_primitive",
                    "check_surv_gte_1")
  
  checks_check <- checks %in% checks_allow
  
  if (any(!checks_check)) {
    stop("The following elements of argument 'checks' are not valid: ",
         paste(checks[!checks_check], collapse = ", "), call. = FALSE)
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
  
  if ("check_NA_A" %in% checks) {
    dat$check_NA_A <- vec_NA_A
  }
  if ("check_NA_U" %in% checks) {
    dat$check_NA_U <- vec_NA_U
  }
  if ("check_NA_F" %in% checks) {
    dat$check_NA_F <- vec_NA_F
  }
  if ("check_NA_C" %in% checks) {
    dat$check_NA_C <- vec_NA_C
  }
  if ("check_zero_U" %in% checks) {
    dat$check_zero_U <- vapply(matU, function(x) all(x == 0 | is.na(x)), FALSE)
  }
  
  if ("check_singular_U" %in% checks) {
    dat$check_singular_U <- mapply(
      CheckMats,
      has_na = vec_NA_U,
      mat = matU,
      MoreArgs = list(fn = CheckSingular)
    )
  }
  
  if ("check_component_sum" %in% checks) {
    dat$check_component_sum <- mapply(ComponentSum, matA, matU, matF, matC)
  }
  
  if ("check_ergodic" %in% checks) {
    dat$check_ergodic <- mapply(
      CheckMats,
      has_na = vec_NA_A,
      mat = matA,
      MoreArgs = list(fn = isErgodic)
    )
  }
  
  if ("check_irreducible" %in% checks) {
    dat$check_irreducible <- mapply(
      CheckMats,
      has_na = vec_NA_A,
      mat = matA,
      MoreArgs = list(fn = isIrreducible)
    )
  }
  
  if ("check_primitive" %in% checks) {
    dat$check_primitive <- mapply(
      CheckMats,
      has_na = vec_NA_A,
      mat = matA,
      MoreArgs = list(fn = isPrimitive)
    )
  }

  maxifnotNAs <- function(x) {
    if (sum(is.na(x)) == length(x)) {
      return(NA)
    } else {
      return(max(x, na.rm = TRUE))
    }
  }
  
  if ("check_surv_gte_1" %in% checks) {
    dat$check_surv_gte_1 <- sapply(matU,maxifnotNAs)>=1
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
  ifelse(("try-error" %in% class(N)) && grepl("singular", N[1]),
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
