#' Identify reproductive stages
#' 
#' Takes a fecundity matrix and returns a vector of reproductive and non-
#' reproductive stages. This is a preparatory step to collapsing the matrix
#' model into combined propagule, pre-reproductive, reproductive and
#' post-reproductive stages.
#'
#' @export
#' @param matF fecundity matrix
#' @return A numeric vector with zeros corresponding to non-reproductive stages
#' and ones corresponding to reproductive stages
#' @keywords ~~
#' @examples
#' matF <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'                  0, 0, 0, 0, 1.0409262, 0.5040727, 0.016433, 0),
#'                  nrow = 5, byrow = FALSE)
#' identifyReproStages(matF)
#' @export identifyReproStages
identifyReproStages <- function(matF) {
  if (all(is.na(matF))) {
    stop("All elements of matF are NA")
  } else if (any(is.na(matF))) {
    ## Assume that NAs correspond to unknown fecundities; replace with Inf so
    ## that reproductive stages are correctly identified.
    matF[which(is.na(matF))] <- Inf
  }
  
  Rep <- apply(matF, 2, function(x) ifelse(any(x > 0) > 0, 1, 0))
  return(Rep)
}