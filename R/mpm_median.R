#' Calculate a median over a list of matrices or CompadreMat objects
#'
#' Calculates an element-wise median over a list of matrices or
#' CompadreMat objects of constant dimension.
#' 
#' The difference between function \code{mat_median}) and (\code{mpm_median} is that
#' \code{mat_median} takes input as a list of matrices (e.g., a list of **A**
#' matrices) while \code{mat_median} takes input as a list of `CompadreMat` objects and
#' thus calculates the mean matrices for both the **A** matrix and its
#' submatrices (**U**, **F**, **C**).
#'
#' @param x A list of matrices or, for \code{mpm_sd} a list of `CompadreMat` objects,
#'   all of the same dimension
#' @param na.rm Logical indicating whether missing values should be excluded
#'   (see \emph{Details}). Defaults to \code{FALSE}.
#'
#' @return A matrix containing the median of each element across all matrices in
#'   the list
#'
#' @details
#' If \code{na.rm == TRUE}, missing values are ignored in the calculation of the
#' mean matrix. If \code{na.rm == TRUE} and a given element is \code{NA} in
#' \emph{every} matrix within \code{x}, the value returned for that element will
#' be \code{0}.
#'
#' @author Darren Norris
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#'
#' @family data management
#' 
#' @name mpm_median
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
#' mat_median(mat_list)
#'
#' # If the matrices are in an RCompadre object, extract them using `matA` before
#' # passing to `mat_median`
#' my_compadre <- cdb_build_cdb(mat_a = mat_list)
#' mat_median(matA(my_compadre))
NULL

#' @rdname mpm_median
#' @importFrom stats median
#' @export
mat_median <- function(x, na.rm = FALSE) {
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


#' @rdname mpm_median
#' @importFrom methods new
#' @importFrom stats median
#' @export
mpm_median <- function(x, na.rm = FALSE) {
  if(!inherits(x, "list")){
    stop("x must be a list of CompadreMat objects")
  }
  if(!inherits(x[[1]], "CompadreMat")){
    stop("x must be a list of CompadreMat objects")
  }
  
  #Use lapply to get matrices, stages when x is a list of compadre objects
  matA <- lapply(x, function(m) m@matA)
  matU <- lapply(x, function(m) m@matU)
  matF <- lapply(x, function(m) m@matF)
  matC <- lapply(x, function(m) m@matC)
  stage_org <- lapply(x, function(m) m@matrixClass$MatrixClassOrganized)
  stage_aut <- lapply(x, function(m) m@matrixClass$MatrixClassAuthor)
  
  stage_org_col <- vapply(stage_org, paste, collapse = " ", "")
  stage_aut_col <- vapply(stage_aut, paste, collapse = " ", "")
  if (length(unique(stage_org_col)) != 1L) {
    warning(
      "CompadreMat objects in given list do not all have the same ",
      "MatrixClassOrganized. Returning MatrixClassOrganized from ",
      "first list element"
    )
  }
  if (length(unique(stage_aut_col)) != 1L) {
    warning(
      "CompadreMat objects in given list do not all have the same ",
      "MatrixClassAuthor. Returning MatrixClassAuthor from first ",
      "list element"
    )
  }
  
  medianA <- mat_median(matA, na.rm = na.rm)
  medianU <- mat_median(matU, na.rm = na.rm)
  medianF <- mat_median(matF, na.rm = na.rm)
  medianC <- mat_median(matC, na.rm = na.rm)
  
  new("CompadreMat",
      matA = medianA,
      matU = medianU,
      matF = medianF,
      matC = medianC,
      matrixClass = x[[1]]@matrixClass
  )
}
