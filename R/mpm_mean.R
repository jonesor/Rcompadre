#' Calculate a mean over a list of matrices or CompadreMat objects
#' 
#' @description
#' Calculates an element-wise mean over a list of matrices or CompadreMat
#' objects of constant dimension.
#' 
#' Does not currently have an option to remove \code{NA}.
#'
#' @param x List of matrices (\code{mat_mean}) or list of CompadreMat objects
#'   (\code{mpm_mean}), all of same dimension
#' 
#' @return A matrix (\code{mat_mean}) or a CompadreMat object (\code{mpm_mean}).
#' 
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @name mpm_mean
#' 
#' @examples 
#' # there are four rows for species 'Haplopappus_radiatus' in Compadre
#' mpms <- Compadre$mat[Compadre$SpeciesAuthor == "Haplopappus_radiatus"]
#' mpm_mean(mpms)
#' 
#' # extract list of matA and take mean
#' mats <- matA(mpms)
#' mat_mean(mats)
NULL


#' @rdname mpm_mean
#' @export
mat_mean <- function(x) {
  n_row <- vapply(x, nrow, numeric(1))
  n_col <- vapply(x, ncol, numeric(1))
  if (length(unique(n_row)) != 1 | length(unique(n_col)) != 1) {
    stop("All matrices in list must be of same dimension")
  }
  n <- length(x)
  return(Reduce("+", x) / n)
}



#' @rdname mpm_mean
#' @importFrom methods new
#' @export
mpm_mean <- function(x) {
  matA <- lapply(x, function(m) m@matA)
  matU <- lapply(x, function(m) m@matU)
  matF <- lapply(x, function(m) m@matF)
  matC <- lapply(x, function(m) m@matC)
  stage_org <- lapply(x, function(m) m@matrixClass$MatrixClassOrganized)
  stage_aut <- lapply(x, function(m) m@matrixClass$MatrixClassAuthor)
  
  stage_org_col <- vapply(stage_org, paste, collapse = " ", "")
  stage_aut_col <- vapply(stage_aut, paste, collapse = " ", "")
  if (length(unique(stage_org_col)) != 1L) {
    warning("CompadreMat objects in given list do not all have the same ",
            "MatrixClassOrganized. Returning MatrixClassOrganized from ",
            "first list element")
  }
  if (length(unique(stage_aut_col)) != 1L) {
    warning("CompadreMat objects in given list do not all have the same ",
            "MatrixClassAuthor. Returning MatrixClassAuthor from ",
            "list element")
  }
  
  meanA <- mat_mean(matA)
  meanU <- mat_mean(matU)
  meanF <- mat_mean(matF)
  meanC <- mat_mean(matC)
  
  new("CompadreMat",
      matA = meanA,
      matU = meanU,
      matF = meanF,
      matC = meanC,
      matrixClass = x[[1]]@matrixClass)
}

