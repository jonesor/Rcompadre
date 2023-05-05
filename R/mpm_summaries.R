#' Calculate summaries over a list of matrices
#'
#' Calculates an element-wise summary over a list of matrices of constant
#' dimension.
#'
#' @param x List of matrices all of same dimension
#' @param na.rm Logical indicating whether missing values should be excluded
#'   (see \emph{Details}). Defaults to \code{FALSE}.
#' @param fun_name Name of function to use. Median "mpm_median" or 
#' standard deviation "mpm_sd". Defaults to \code{NA}.
#'
#' @return A matrix containing the summary of each element across all matrices in
#'   the list
#'
#' @details
#' Currently available functions to calculate median "mpm_median" or 
#' standard deviation "mpm_sd".
#' If \code{na.rm == TRUE}, missing values are ignored in the calculation of the
#' mean matrix. If \code{na.rm == TRUE} and a given element is \code{NA} in
#' \emph{every} matrix within \code{x}, the value returned for that element will
#' be \code{0}.
#' 
#' @examples
#' # set seed for repeatability
#' set.seed(42)
#' 
#' # create a function that generates a matrix with random values
#' create_matrix <- function() {
#'   matrix(runif(9, 0, 1), nrow = 3)
#' }
#' 
#' # use replicate() to call the create_matrix() function 20 times
#' mat_list <- replicate(20, create_matrix(), simplify = FALSE)
#' 
#' # get the median matrix
#' mpm_summaries(mat_list, fun_name = "mpm_median")
#' 
#' # get the standard deviation matrix
#' mpm_summaries(mat_list, fun_name = "mpm_sd")
#' 
#' # If the matrices are in an RCompadre object, extract them using `matA` before
#' # passing to `mpm_summaries`
#' my_compadre <- cdb_build_cdb(mat_a = mat_list)
#' mpm_summaries(matA(my_compadre), fun_name = "mpm_median")
#'
#' @author Darren Norris
#'
#' @family data management
#' @importFrom stats median sd
#'
#' @export mpm_summaries
mpm_summaries <- function(x, na.rm = FALSE, fun_name = NA) {
  n_row <- vapply(x, nrow, numeric(1))
  n_col <- vapply(x, ncol, numeric(1))
  if (length(unique(n_row)) != 1 || length(unique(n_col)) !=
    1) {
    stop("All matrices in list must be of same dimension")
  }
  
  if (is.na(fun_name) || length(unique(fun_name)) != 1) {
    stop(cat(c("fun_name must be one of \"mpm_median\", \"mpm_sd\"")))
  }
  
  if (na.rm) {
    x <- lapply(x, zero_NA)
  }
  n <- length(x)
  rc <- dim(x[[1]])
  rownames_mat <- rownames(x[[1]])
  colnames_mat <- colnames(x[[1]])
  ar1 <- array(unlist(x), c(rc, n))
# Calculate desired summaries  
  if(fun_name == "mpm_median") {
  mat_out <- apply(ar1, c(1, 2), median) 
  } else if (fun_name == "mpm_sd") {
    mat_out <- apply(ar1, c(1, 2), sd)
  } else {
    mat_out <- x[[1]]
    mat_out <- NA
  }
# Return results
  rownames(mat_out) <- rownames_mat
  colnames(mat_out) <- colnames_mat
  return(mat_out)
}
