#' Calculate a standard deviation over a list of matrices
#'
#' Calculates an element-wise standard deviation over a list of matrices of
#' constant dimension.
#'
#' @param x List of matrices all of same dimension
#' @param na.rm Logical indicating whether missing values should be excluded
#'   (see \emph{Details}). Defaults to \code{FALSE}.
#'
#' @return A matrix containing the standard deviation of each element across all
#'   matrices in the list
#'
#' @details
#' If \code{na.rm == TRUE}, missing values are ignored in the calculation of the
#' mean matrix. If \code{na.rm == TRUE} and a given element is \code{NA} in
#' \emph{every} matrix within \code{x}, the value returned for that element will
#' be \code{0}.
#'
#' @author Darren Norris
#'
#' @family data management
#' @importFrom stats median
#'
#' @export mpm_sd
mpm_median <- function(x, na.rm = FALSE) {
  n_row <- vapply(x, nrow, numeric(1))
  n_col <- vapply(x, ncol, numeric(1))
  if (length(unique(n_row)) != 1 || length(unique(n_col)) !=
    1) {
    stop("All matrices in list must be of same dimension")
  }
  if (na.rm) {
    x <- lapply(x, zero_NA)
  }
  n <- length(x)
  rc <- dim(x[[1]])
  rownames_mat <- rownames(x[[1]])
  colnames_mat <- colnames(x[[1]])
  ar1 <- array(unlist(x), c(rc, n))
  mat_out <- apply(ar1, c(1, 2), median)
  rownames(mat_out) <- rownames_mat
  colnames(mat_out) <- colnames_mat
  return(mat_out)
}


#' Calculate a median over a list of matrices
#'
#' Calculates an element-wise median over a list of matrices of
#' constant dimension.
#'
#' @param x List of matrices all of same dimension
#' @param na.rm Logical indicating whether missing values should be excluded
#'   (see \emph{Details}). Defaults to \code{FALSE}.
#'
#' @return A matrix containing the median of each element across all
#'   matrices in the list
#'
#' @details
#' If \code{na.rm == TRUE}, missing values are ignored in the calculation of the
#' mean matrix. If \code{na.rm == TRUE} and a given element is \code{NA} in
#' \emph{every} matrix within \code{x}, the value returned for that element will
#' be \code{0}.
#'
#' @author Darren Norris
#'
#' @family data management
#'
#' @importFrom stats sd
#' @export mpm_sd
mpm_sd <- function(x, na.rm = FALSE) {
  n_row <- vapply(x, nrow, numeric(1))
  n_col <- vapply(x, ncol, numeric(1))
  if (length(unique(n_row)) != 1 || length(unique(n_col)) !=
    1) {
    stop("All matrices in list must be of same dimension")
  }
  if (na.rm) {
    x <- lapply(x, zero_NA)
  }
  n <- length(x)
  rc <- dim(x[[1]])
  rownames_mat <- rownames(x[[1]])
  colnames_mat <- colnames(x[[1]])
  ar1 <- array(unlist(x), c(rc, n))
  mat_out <- apply(ar1, c(1, 2), sd)
  rownames(mat_out) <- rownames_mat
  colnames(mat_out) <- colnames_mat
  return(mat_out)
}
