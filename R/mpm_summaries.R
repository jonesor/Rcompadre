#' Calculate summaries over a list of matrices
#'
#' Calculates an element-wise summary over a list of matrices of constant
#' dimension.
#'
#' @param x List of matrices all of same dimension
#' @param na.rm Logical indicating whether missing values should be excluded
#'   (see \emph{Details}). Defaults to \code{FALSE}.
#' @param fun_name Name of function to use. Mean "mpm_mean2", median "mpm_median" or 
#' standard deviation "mpm_sd". Defaults to \code{NA}.
#'
#' @return A matrix containing the summary of each element across all matrices in
#'   the list
#'
#' @details
#' Currently available options are to calculate mean  "mpm_mean2", median "mpm_median" or 
#' standard deviation "mpm_sd" over a list of matrices of constant dimension.
#' Intended for use with stage based population model matrices that are typically small i.e. 
#' dimensions less than 50 columns and rows. Unlikely to be efficient for long lists 
#' of large matrices.
#' 
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
#' # get the standard deviation matrix
#' mpm_summaries(mat_list, fun_name = "mpm_sd")
#' # get the mean matrix
#' mpm_summaries(mat_list, fun_name = "mpm_mean2")
#' 
#' # If the matrices are in an RCompadre object, extract them using `matA` before
#' # passing to `mpm_summaries`
#' my_compadre <- cdb_build_cdb(mat_a = mat_list)
#' mpm_summaries(matA(my_compadre), fun_name = "mpm_median")
#'
#' @author Darren Norris
#'
#' @family data management
#' @importFrom stats median sd mean
#'
#' @export mpm_summaries
mpm_summaries <- function(x, na.rm = FALSE, fun_name = NA) {
# Check matrix dimensions
  n_row <- vapply(x, nrow, numeric(1))
  n_col <- vapply(x, ncol, numeric(1))
  if (length(unique(n_row)) != 1 || length(unique(n_col)) !=
    1) {
    stop("All matrices in list must be of same dimension.")
  }
# Check summary function 
valid_functions <- c("mpm_median", "mpm_mean2", "mpm_sd")
  if (is.na(fun_name)) {
    stop("Option fun_name is missing. 
         fun_name must be one of \"mpm_median\", \"mpm_mean2\", \"mpm_sd\".")
  }
  
  if (length(unique(fun_name)) != 1 || !fun_name %in% valid_functions) {
    stop("fun_name must be one of \"mpm_median\", \"mpm_mean2\", \"mpm_sd\".")
  }
# Check NA
  if (na.rm) {
    x <- lapply(x, zero_NA)
  }
# Get dimensions and names
  n <- length(x)
  rc <- dim(x[[1]])
  rownames_mat <- rownames(x[[1]])
  colnames_mat <- colnames(x[[1]])
  ar1 <- array(unlist(x), c(rc, n))
# Calculate desired summary  
  if(fun_name == "mpm_median") {
  mat_out <- apply(ar1, c(1, 2), median) 
  } else if (fun_name == "mpm_sd") {
    mat_out <- apply(ar1, c(1, 2), sd)
  } else if (fun_name == "mpm_mean2") {
    mat_out <- apply(ar1, c(1, 2), mean)
  } else {
    mat_out <- NA
  }
# Return results
  rownames(mat_out) <- rownames_mat
  colnames(mat_out) <- colnames_mat
  return(mat_out)
}
