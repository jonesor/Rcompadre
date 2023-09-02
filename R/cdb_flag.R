#' Flag potential issues in matrices of a COM(P)ADRE database
#'
#' @description
#' Adds columns to the data slot of a `CompadreDB` object that flag potential
#' problems in the matrix population models. These columns can subsequently be
#' used to subset the database by logical argument.
#'
#' Optional checks include:
#' \itemize{
#'   \item \code{check_NA_A}: missing values in `matA`? Missing (`NA`) values in
#'   matrices prevent most calculations using those matrices.
#'   \item \code{check_NA_U}: missing values in `matU`? Missing (`NA`) values in
#'   matrices prevent most calculations using those matrices.
#'   \item \code{check_NA_F}: missing values in `matF`? Missing (`NA`) values in
#'   matrices prevent most calculations using those matrices.
#'   \item \code{check_NA_C}: missing values in `matC`? Missing (`NA`) values in
#'   matrices prevent most calculations using those matrices.
#'   \item \code{check_zero_U}: `matU` all zeros (including `NA`)? Submatrices
#'   composed entirely of zero values can be problematic. There may be good
#'   biological reasons for this phenomenon. For example, in the particular
#'   focal population in the particular focal year, there was truly zero
#'   survival recorded. Nevertheless, zero-value submatrices can cause some
#'   calculations to fail and it may be necessary to exclude them.
#'   \item \code{check_zero_F}: `matF` all zeros (including `NA`)? Submatrices
#'   composed entirely of zero values can be problematic. There may be good
#'   biological reasons for this phenomenon. For example, in the particular
#'   focal population in the particular focal year, there was truly zero
#'   reproduction recorded. Nevertheless, zero-value submatrices can cause some
#'   calculations to fail and it may be necessary to exclude them.
#'   \item \code{check_zero_U_colsum}: Columns of `matU` that sum to zero imply
#'   that there is is no survival from that particular stage. This may be a
#'   perfectly valid parameterisation for a particular year/place but is
#'   biologically unreasonable in the longer term and users may wish to exclude
#'   problematic matrices from their analysis.
#'   \item \code{check_singular_U}: `matU` singular? Matrices are said to be
#'   singular if they cannot be inverted. Inversion is required for many matrix
#'   calculations and, therefore, singularity can cause some calculations to
#'   fail.
#'   \item \code{check_component_sum}: do `matU`/`matF`/`matC` submatrices sum
#'   to `matA` (see \emph{Details})? A complete MPM (`matA`) can be split into
#'   its component submatrices (i.e., `matU`, `matF` and  `matC`). The sum of
#'   these submatrices should equal the complete MPM (i.e., `matA` = `matU` +
#'   `matF` + `matC`). Sometimes, however, errors occur so that the submatrices
#'   do NOT sum to `matA`. Normally, this is caused by rounding errors, but more
#'   significant errors are possible.
#'   \item \code{check_ergodic}: is `matA` ergodic (see
#'   \code{\link[popdemo]{isErgodic}})? Some matrix calculations require that
#'   the MPM (`matA`) be ergodic. Ergodic MPMs are those where there is a single
#'   asymptotic stable state that does not depend on initial stage structure.
#'   Conversely, non-ergodic MPMs are those where there are multiple asymptotic
#'   stable states, which depend on initial stage structure. MPMs that are
#'   non-ergodic are usually biologically unreasonable, both in terms of their
#'   life cycle description and their projected dynamics. They cause some
#'   calculations to fail.
#'   \item \code{check_irreducible}: is `matA` irreducible (see
#'   \code{\link[popdemo]{isIrreducible}})? Some matrix calculations require
#'   that the MPM (`matA`) be irreducible. Irreducible MPMs are those where
#'   parameterised transition rates facilitate pathways from all stages to all
#'   other stages. Conversely, reducible MPMs depict incomplete life cycles
#'   where pathways from all stages to every other stage are not possible. MPMs
#'   that are reducible are usually biologically unreasonable, both in terms of
#'   their life cycle description and their projected dynamics. They cause some
#'   calculations to fail. Irreducibility is necessary but not sufficient for
#'   ergodicity.
#'   \item \code{check_primitive}: is `matA` primitive (see
#'   \code{\link[popdemo]{isPrimitive}})? A primitive matrix is non-negative
#'   matrix that is irreducible and has only a single eigenvalue of maximum
#'   modulus. This check is therefore redundant due to the overlap with
#'   `check_irreducible` and `checkErdogic`.
#'   \item \code{check_surv_gte_1}: does `matU` contains values that are equal
#'   to or greater than 1? Survival is bounded between 0 and 1. Values in excess
#'   of 1 are biologically unreasonable.
#' }
#'
#' @param cdb A CompadreDB object
#' @param checks Character vector specifying which checks to run.
#'
#'   Defaults to all, i.e. \code{c("check_NA_A", "check_NA_U", "check_NA_F",
#'   "check_NA_C", "check_zero_U", "check_singular_U", "check_component_sum",
#'   "check_ergodic", "check_irreducible", "check_primitive",
#'   "check_surv_gte_1")}
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
#' @references Stott, I., Townley, S., & Carslake, D. 2010. On reducibility and
#'   ergodicity of population projection matrix models. Methods in Ecology and
#'   Evolution. 1 (3), 242-252
#'
#' @examples
#' CompadreFlag <- cdb_flag(Compadre)
#'
#' # only check whether matA has missing values, and whether matA is ergodic
#' CompadreFlag <- cdb_flag(Compadre, checks = c("check_NA_A", "check_ergodic"))
#' @importFrom popdemo isErgodic isIrreducible isPrimitive
#' @importFrom methods new
#' @export cdb_flag
cdb_flag <- function(cdb, checks = c(
                       "check_NA_A",
                       "check_NA_U",
                       "check_NA_F",
                       "check_NA_C",
                       "check_zero_U",
                       "check_zero_F",
                       "check_zero_C",
                       "check_zero_U_colsum",
                       "check_singular_U",
                       "check_component_sum",
                       "check_ergodic",
                       "check_irreducible",
                       "check_primitive",
                       "check_surv_gte_1"
                     )) {
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }

  checks_allow <- c(
    "check_NA_A",
    "check_NA_U",
    "check_NA_F",
    "check_NA_C",
    "check_zero_U",
    "check_zero_F",
    "check_zero_C",
    "check_zero_U_colsum",
    "check_singular_U",
    "check_component_sum",
    "check_ergodic",
    "check_irreducible",
    "check_primitive",
    "check_surv_gte_1"
  )

  checks_check <- checks %in% checks_allow

  if (!any(checks_check)) {
    stop("The following elements of argument 'checks' are not valid: ",
      toString(checks[!checks_check]),
      call. = FALSE
    )
  }

  dat <- cdb@data

  matA <- matA(cdb)
  matU <- matU(cdb)
  matF <- matF(cdb)
  matC <- matC(cdb)

  # calculate outside conditionals because may be required later
  vec_NA_A <- vapply(matA, function(x) anyNA(x), FALSE)
  vec_NA_U <- vapply(matU, function(x) anyNA(x), FALSE)
  vec_NA_F <- vapply(matF, function(x) anyNA(x), FALSE)
  vec_NA_C <- vapply(matC, function(x) anyNA(x), FALSE)

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
  if ("check_zero_F" %in% checks) {
    dat$check_zero_F <- vapply(matF, function(x) all(x == 0 | is.na(x)), FALSE)
  }
  if ("check_zero_C" %in% checks) {
    dat$check_zero_C <- vapply(matC, function(x) all(x == 0 | is.na(x)), FALSE)
  }
  if ("check_zero_U_colsum" %in% checks) {
    dat$check_zero_U_colsum <- vapply(
      matU,
      function(x) {
        any(colSums(x,
          na.rm = TRUE
        ) == 0)
      }, FALSE
    )
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
    dat$check_component_sum <- unlist(Map(ComponentSum,
      mA = matA,
      mU = matU,
      mF = matF,
      mC = matC
    ))
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
    dat$check_surv_gte_1 <- vapply(matU, maxifnotNAs, numeric(1)) >= 1
  }

  new("CompadreDB",
    data = dat,
    version = cdb@version
  )
}



# utilities

#' Check matrices for NA values and apply a function
#'
#' This function checks a matrix for NA values and applies a specified function
#' to it. If the matrix contains any NA values, the function returns NA.
#'
#' @param has_na A logical value indicating whether the matrix may contain NA
#'   values
#' @param mat A matrix
#' @param fn A function to apply to the matrix
#'
#' @return The result of applying the function to the matrix, or NA if the
#'   matrix contains any NA values
#' @keywords internal
#' @noRd
CheckMats <- function(has_na, mat, fn) {
  fn <- match.fun(fn)
  ifelse(has_na, NA, fn(mat))
}

#' Check if a matrix is singular
#'
#' This function checks if a matrix is singular by attempting to calculate its
#' fundamental matrix using the \code{\link{solve}} function. If the matrix is
#' singular, the function returns `TRUE`.
#' s
#' @param matU A matrix
#'
#' @return A logical value indicating whether the matrix is singular
#' @keywords internal
#' @noRd
CheckSingular <- function(matU) {
  # try calculating fundamental matrix
  N <- try(solve(diag(nrow(matU)) - matU), silent = TRUE)

  # flag if singular
  out <- inherits(N, "try-error") && grepl("singular", N[1], fixed = TRUE)
  return(out)
}

#' Calculate the sum of three matrices and compare to a reference matrix
#'
#' This function calculates the sum of three matrices (`mU`, `mF`, and `mC`) and
#' compares it to a reference matrix (`mA`). If the sum of the three matrices is
#' equal to the reference matrix, the function returns `TRUE.` Otherwise, it
#' returns `FALSE` or `NA` if the sum of the three matrices is zero or contains
#' NA values.
#'
#' @param mA A matrix
#' @param mU A matrix
#' @param mF A matrix
#' @param mC A matrix
#'
#' @return A logical value indicating whether the sum of the three matrices is
#'   equal to the reference matrix
#' @keywords internal
#' @noRd
ComponentSum <- function(mA, mU, mF, mC) {
  mat_dim <- nrow(mA)

  if (all(is.na(mU))) {
    mU <- matrix(0, mat_dim, mat_dim)
  }
  if (all(is.na(mF))) {
    mF <- matrix(0, mat_dim, mat_dim)
  }
  if (all(is.na(mC))) {
    mC <- matrix(0, mat_dim, mat_dim)
  }

  mat_sum <- mU + mF + mC

  if (all(mat_sum == 0 | is.na(mat_sum))) {
    out <- NA
  } else {
    val_check <- unlist(Map(
      function(x, y) {
        isTRUE(all.equal(x, y))
      },
      x = mat_sum, y = mA
    ))

    out <- all(val_check)
  }

  return(out)
}
