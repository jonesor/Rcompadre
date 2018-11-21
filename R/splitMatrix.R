#' Splits a matrix population model into the constituent U, F and C matrices.
#'
#' This function splits a matrix population model into three constituent matrices, U (growth and survival processes), F (sexual reproduction) and C (clonal reproduction).
#' Warning! The functionality is very basic - it assumes that sexual reproduction is located in the top row of the matrix, and that everything else is growth or survival (the U matrix). Clonality is assumed to be non-existant.
#'
#' @param CompadreMat a CompadreMat object. If this argument is not empty, then 
#'   matA is extracted from the CompadreMat object, and
#'   any object passed to matA is ignored.
#' @param matA A matrix population model.
#' 
#' @return A list of three matrices: `matU`,`matF` and `matC`.
#' 
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' 
#' @examples
#' \dontrun{library(popdemo)
#' data(Tort)
#' splitMatrix(Tort)
#' }
#' 
#' @export splitMatrix
#' 
splitMatrix <- function(matA){
  if(!is.null(CompadreMat)){
    if(!class(CompadreMat) %in% "CompadreMat") stop("CompadreMat must be a CompadreMat object")
    if(!is.null(matA)) warning("Extracting matU from CompadreMat, ignored given matU")
    matA <- matA(CompadreMat)
  }
  matU <- matA
  matU[1,] <- rep(0,ncol(matA))
  matF <- matrix(c(matA[1,],rep(0,ncol(matA)*(nrow(matA)-1))),ncol=ncol(matA),byrow = TRUE)
  matC <- matrix(rep(0,ncol(matA)^2),ncol=ncol(matA))
  return(list(matU, matF, matC)) 
}