#' Apply a function element-wise to a list of matrices
#'
#' This function applies a specified function element-wise to the corresponding
#' elements across a list of matrices.
#'
#' @param matrix_list A list of matrices.
#' @param fun The function to apply to the elements.
#' @param na_handling A character string specifying how to handle NA values.
#'   Possible values are "stop" (throw an error when NA values are encountered),
#'   "zero" (convert NA values to 0), and "ignore" (NA values are ignored and
#'   passed to `fun`). Handling can then be processed apprpriately by that
#'   function (e.g., with na.rm).
#'
#' @return A matrix containing the result of applying the function element-wise
#'   to the corresponding elements across the matrices.
#'   
#' @name mat_elementwise_apply
#' @family data management
#' 
#' @examples
#' mpms <- Compadre$mat[Compadre$SpeciesAuthor == "Haplopappus_radiatus"]
#' 
#' #The object mpms is a list, containing compadre objects
#' class(mpms)
#' class(mpms[[1]])
#'
#' # extract list of matA and take mean
#' mats <- matA(mpms)
#' mat_elementwise_apply(mats, mean)
#'
#' # This should be the same as mat_mean()
#' mat_mean(mats)
#'
#' # Mean values, with 25% trimmed from each end
#' mat_elementwise_apply(mats, mean, trim = 0.25)
#'
#' # weighted mean, where the second matrix is weighted to 100% and the others to 0%
#' # do demonstrate usage. The result should be the same as mats[[2]]
#' mat_elementwise_apply(mats, weighted.mean, w = c(0,1,0,0))
#' mats[[2]]
#'
#' #min and max values
#' mat_elementwise_apply(mats, min)
#' mat_elementwise_apply(mats, max)
NULL

#' @rdname mat_elementwise_apply
#' @importFrom stats sd
#' @export
mat_elementwise_apply <- function(matrix_list, fun, na_handling = "stop", ...) {
  # Input validation
  if (!is.list(matrix_list) || any(!sapply(matrix_list, is.matrix))) {
    stop("Input must be a list of matrices.")
  }
  
  if (!is.function(fun)) {
    stop("The 'fun' parameter must be a function.")
  }
  
  valid_na_handling <- c("stop", "ignore", "zero")
  if (!(na_handling %in% valid_na_handling)) {
    stop("Invalid 'na_handling' option. Supported options are: ", paste(valid_na_handling, collapse = ", "))
  }
  
  # Get the dimension of the matrices
  n <- nrow(matrix_list[[1]])
  
  # Apply the function to each element across matrices
  result <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      elements <- sapply(matrix_list, "[", i, j)
      
      # Handle NA values based on the specified option
      if (na_handling == "stop" && any(is.na(elements))) {
        stop("NA values encountered in the matrices.")
      } else if (na_handling == "zero") {
        elements[is.na(elements)] <- 0
      } else if (na_handling == "ignore") {

      }
      
      result[i, j] <- fun(elements, ...)
    }
  }
  
  result
}