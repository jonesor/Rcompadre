#' Apply a function element-wise to a list of matrices
#'
#' This function applies a specified function element-wise to the corresponding
#' elements across a list of matrices.
#'
#' @param x A list of matrices.
#' @param fun The function to apply to the elements.
#' @param na_handling A character string specifying how to handle NA values.
#'   Possible values are "stop" (throw an error when NA values are encountered),
#'   "zero" (convert NA values to 0), and "ignore" (NA values are ignored and
#'   passed to `fun`). Handling can then be processed appropriately by that
#'   function (e.g., with `na.rm`).
#' @param ... Additional arguments passed to `fun`.
#'
#' @return A matrix containing the result of applying the function element-wise
#'   to the corresponding elements across the matrices.
#'   
#' @name mpm_elementwise_apply
#' @family data management
#' 
#' @examples
#' mpms <- Compadre$mat[Compadre$SpeciesAuthor == "Haplopappus_radiatus"]
#' 
#' #The object mpms is a list, containing compadre objects
#' class(mpms)
#' class(mpms[[1]])
#' 
#' # Get the mean, max and min for the matrices
#' mpm_elementwise_apply(mpms, mean)
#' mpm_elementwise_apply(mpms, max)
#' mpm_elementwise_apply(mpms, min)
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
#'
#' #Demonstrating NA handling
#' #First adding some NA values to the matrices
#' mats[[2]][3,2] <- NA
#' 
#' #replace the NA with a 0
#' mat_elementwise_apply(mats, min, na_handling = "zero")
#'
#' #ignore the NA
#' mat_elementwise_apply(mats, min, na_handling = "ignore")
#'
#' #ignore the NA, but pass na.rm = TRUE to the function (min)
#' mat_elementwise_apply(mats, min, na_handling = "ignore", na.rm = TRUE)
NULL

#' @rdname mpm_elementwise_apply
#' @importFrom stats sd
#' @export
mat_elementwise_apply <- function(x, fun, na_handling = "stop", ...) {
  # Input validation
  if (!is.list(x) || any(!sapply(x, is.matrix))) {
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
  n <- nrow(x[[1]])
  
  # Apply the function to each element across matrices
  result <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      elements <- sapply(x, "[", i, j)
      
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

#' @rdname mpm_elementwise_apply
#' @importFrom methods new
#' @export
mpm_elementwise_apply <- function(x, fun, na_handling = "stop", ...) {
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

  summaryA <- mat_elementwise_apply(matA, fun = fun, na_handling = na_handling, ...)
  summaryU <- mat_elementwise_apply(matU, fun = fun, na_handling = na_handling, ...)
  summaryF <- mat_elementwise_apply(matF, fun = fun, na_handling = na_handling, ...)
  summaryC <- mat_elementwise_apply(matC, fun = fun, na_handling = na_handling, ...)
  
  new("CompadreMat",
      matA = summaryA,
      matU = summaryU,
      matF = summaryF,
      matC = summaryC,
      matrixClass = x[[1]]@matrixClass
  )
}
