#' Converts a matrix in character string format to numeric matrix format
#' 
#' This function converts a matrix in character string format (e.g. "[0.2 0.3
#' 0.1 0]") to a square numeric matrix.
#' 
#' @param A A square matrix in the form of a string, begining with an open
#'   square bracket and ending with a closed square bracket, with individual
#'   matrix cell entries separated by a space.
#' @return A square numeric \code{matrix}.
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @seealso convert2flat
#' @examples
#' x1 <- "[3.3 5.2 6.1 0.1 NA 0.3 0.2 0.4 0.1]"
#' stringToMatrix(x1)
#' 
#' x2 <- "[0 0 0 0.42 0.65 0 0 0 0 0.52 0 0 0 0 0.2 0]"
#' stringToMatrix(x2)
#' 
#' \dontrun{
#' # non-square matrix
#' x3 <- "[0.42 0.52 0.15 0.23 0.14]"
#' stringToMatrix(x3)
#' }
#' @export
stringToMatrix <- function(A) {
  A <- gsub(pattern = "\\[|\\]", "", A)
  A <- gsub(pattern = ";", " ", A)
  A <- strsplit(x = A, split = " |`NA`")[[1]]
  A[which(A == 'NA')] <- NA
  
  # check whether A will form square matrix
  matDim <- sqrt(length(A))
  if(abs(as.integer(matDim) - matDim) > 0.00000001) {
    stop("This matrix is not square")
  }
  
  # convert to matrix and return
  mat <- matrix(as.numeric(A), nrow = sqrt(length(A)), byrow = TRUE)
  return(mat)
}
