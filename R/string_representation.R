#' Convert vectors or square numeric matrices to and from string representation
#'
#' @description
#' Functions to convert vectors or square numeric matrices to and from string
#' representation, which is primarily useful for writing data frames with
#' list-columns containing vectors or matrices to a flat file format such as
#' csv.
#'
#' String representations of vectors and matrices begin with an open bracket
#' ("[") and end with a closed bracket ("]"). Matrix elements are separated with
#' a space ("[0.2 0.3 0.1 0]") whereas vector elements are separate with two
#' vertical bars ("[Seedling||Juvenile||Reproductive]").
#'
#' @name string_representation
#'
#' @param mat A square numeric matrix
#' @param vec A vector
#' @param mat_str A square numeric matrix in string representation
#' @param vec_str A vector in string representation
#' @param numeric Logical value indicating whether a string representation of a
#'   vector should be coerced to numeric (if FALSE remains character)
#'
#' @return
#' A square numeric matrix (\code{string_to_mat}), vector
#' (\code{string_to_vec}), or string (\code{mat_to_string} or
#' \code{vec_to_string}).
#'
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @author Patrick M. Barks <patrick.barks@@gmail.com>
#'
#' @family data management
#'
#' @seealso \link{cdb_flatten} \link{cdb_unflatten}
#'
#' @examples
#' mat_str <- "[3.3 5.2 6.1 0.1 NA 0.3 0.2 0.4 0.1]"
#' mat <- string_to_mat(mat_str)
#'
#' vec1_str <- "[0.30||0.42||0.19||0.09]"
#' vec1 <- string_to_vec(vec1_str, numeric = TRUE)
#'
#' vec2_str <- "[Seedling 1||Seedling 2||Juvenile||Reproductive]"
#' vec2 <- string_to_vec(vec2_str)
#'
#' # convert back to string format
#' mat_to_string(mat)
#' vec_to_string(vec1)
#' vec_to_string(vec2)
#'
#' \dontrun{
#' # non-square matrix
#' mat_str <- "[0.42 0.52 0.15 0.23 0.14]"
#' string_to_mat(mat_str)
#' }
NULL



#' @rdname string_representation
#' @export mat_to_string
mat_to_string <- function(mat) {
  if (!is.matrix(mat) || !is.numeric(mat) || (nrow(mat) != ncol(mat))) {
    stop("mat must be a square numeric matrix", call. = FALSE)
  }
  paste0("[", paste(t(mat), collapse = " "), "]")
}


#' @rdname string_representation
#' @export vec_to_string
vec_to_string <- function(vec) {
  paste0("[", paste(t(vec), collapse = "||"), "]")
}


#' @rdname string_representation
#' @export string_to_mat
string_to_mat <- function(mat_str) {
  mat <- gsub("^\\[|\\]$", "", mat_str)
  mat <- gsub(pattern = ";", " ", mat)
  mat <- strsplit(mat, split = "[[:space:]]")[[1]]
  mat[mat == "NA"] <- NA_real_

  # check whether mat will form square matrix
  matDim <- sqrt(length(mat))
  if (abs(as.integer(matDim) - matDim) > 0.00000001) {
    stop("The matrix represented by mat_s does not appear to be square",
      call. = FALSE
    )
  }

  return(matrix(as.numeric(mat), nrow = matDim, byrow = TRUE))
}


#' @rdname string_representation
#' @export string_to_vec
string_to_vec <- function(vec_str, numeric = FALSE) {
  vec <- gsub("^\\[|\\]$", "", vec_str)
  vec <- strsplit(vec, split = "\\|\\|")[[1]]
  vec[vec == "NA"] <- NA

  if (numeric) {
    return(as.numeric(vec))
  } else {
    return(vec)
  }
}
