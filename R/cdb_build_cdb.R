#' Create a CompadreDB object from user-specified data
#'
#' Creates a CompadreDB object from data provided by the user in the form of
#' matrices and metadata. Users can provide either a list of A matrices (i.e.
#' the whole matrix population model) or lists of process-based submatrices
#' \code{matU}, \code{matF}and \code{matC}. In this latter case, we assume that
#' \code{matA = matU + matF + matC}. If only one type of the submatrices are
#' provided, the others are assumed to be 0. If only the A matrices are
#' provided, the submatrices are recorded as `NA`.
#'
#' @param mat_a A `list` of A matrices
#' @param mat_u A `list` of U matrices (representing survival and growth)
#' @param mat_f A `list` of F matrices (representing sexual reproduction)
#' @param mat_c A `list` of C matrices (representing clonal reproduction)
#' @param stages A `list` of stage definitions provided as `data.frame`s that
#'   include two columns: `MatrixClassOrganized` and `MatrixClassAuthor`. If
#'   this argument is not provided, numeric stage names are generated
#'   automatically
#' @param metadata A `data.frame` of metadata associated with each matrix.
#'   Metadata should be provided by row in the same order as the matrices are
#'   placed in the lists.
#' @param version An optional string allowing users to add version information
#'   to their output object. If this argument is not provided the current date
#'   and time is used.
#'
#' @return A valid CompadreDB object
#'
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#'
#' @family data acquisition
#' @importFrom methods hasArg
#'
#' @examples
#' # If you only have A matrices
#'
#' mat_a1 <- rbind(
#'   c(0.1, 1.9),
#'   c(0.5, 0.7)
#' )
#'
#' mat_a2 <- rbind(
#'   c(0.2, 1.4, 2.3),
#'   c(0.6, 0.3, 1.1),
#'   c(0.2, 0.2, 1.5)
#' )
#'
#' mat_a3 <- rbind(
#'   c(0.1, 2.1),
#'   c(0.3, 0.4)
#' )
#'
#' # Place the matrices into a list
#' mat_a_list <- mget(ls(pattern = "mat_a[0-9]"))
#'
#' my_compadre <- cdb_build_cdb(mat_a = mat_a_list, version = "testrun")
#' my_compadre
#'
#'
#' mat_u1 <- rbind(
#'   c(0.1, 0.0),
#'   c(0.5, 0.7)
#' )
#'
#' mat_u2 <- rbind(
#'   c(0.2, 0.0, 0.0),
#'   c(0.6, 0.3, 1.1),
#'   c(0.2, 0.2, 1.5)
#' )
#'
#' mat_f1 <- rbind(
#'   c(0.0, 1.9),
#'   c(0.0, 0.0)
#' )
#'
#' mat_f2 <- rbind(
#'   c(0.0, 1.4, 2.3),
#'   c(0.0, 0.0, 0.0),
#'   c(0.0, 0.0, 0.0)
#' )
#'
#' mat_u_list <- mget(ls(pattern = "mat_u[0-9]"))
#' mat_f_list <- mget(ls(pattern = "mat_f[0-9]"))
#'
#' meta <- data.frame(idNum = 1:2, SpeciesAccepted = c("A", "B"), x = 4:5)
#'
#' stageInfo <- list(
#'   data.frame(
#'     MatrixClassOrganized = rep("active", 2),
#'     MatrixClassAuthor = c("small", "large")
#'   ),
#'   data.frame(
#'     MatrixClassOrganized = rep("active", 3),
#'     MatrixClassAuthor = c("small", "medium", "large")
#'   )
#' )
#'
#'
#' my_compadre <- cdb_build_cdb(
#'   mat_u = mat_u_list, mat_f = mat_f_list,
#'   metadata = meta, stages = stageInfo
#' )
#' my_compadre
#'
#' my_compadre <- cdb_build_cdb(
#'   mat_u = mat_u_list, mat_f = mat_f_list,
#'   metadata = meta
#' )
#' my_compadre
#' @export
#'
cdb_build_cdb <- function(mat_a = NULL, mat_u = NULL, mat_f = NULL,
                          mat_c = NULL, stages = NULL, version = NULL,
                          metadata = NULL) {

  # Check matrices
  # Which matrices are provided
  includedMatrices <- data.frame(
    matType = c(
      "mat_a", "mat_u", "mat_f",
      "mat_c"
    ),
    present = c(
      hasArg(mat_a), hasArg(mat_u),
      hasArg(mat_f), hasArg(mat_c)
    )
  )
  AUFC <- includedMatrices$present

  if (sum(AUFC) == 0) {
    stop("No matrices provided: matrices must be provided as (i) a list of A matrices; (ii) lists of U and F matrices; or (iii) lists of U, F and C matrices.")
  }

  if (hasArg(mat_a) && any(hasArg(mat_u), hasArg(mat_f), hasArg(mat_c))) {
    stop("When mat_a is provided, mat_u, mat_f, and mat_c should NOT be provided,")
  }

  # If mat U is provided, mat F needs be provided.
  if (hasArg(mat_u) && !hasArg(mat_f)) {
    stop("When mat_u is provided, mat_f must also be provided.")
  }

  # If mat C is provided, both mat U and mat F need to be provided.
  if (AUFC[4]) {
    if (sum(AUFC[2:3]) != 2) {
      stop("When mat_c is provided, mat_u and mat_f must also be provided.")
    }
  }

  # mat (Matrices) -----
  # Construct the series of AUFC matrices and put together in a list, for each
  # element

  # If A is provided, make U, F, and C `NA`.
  if (hasArg(mat_a)) {
    mat <- NULL
    for (i in 1:length(mat_a)) {
      mat_u[[i]] <- matrix(nrow = nrow(mat_a[[i]]), ncol = nrow(mat_a[[i]]))
      mat_f[[i]] <- matrix(nrow = nrow(mat_a[[i]]), ncol = nrow(mat_a[[i]]))
      mat_c[[i]] <- matrix(nrow = nrow(mat_a[[i]]), ncol = nrow(mat_a[[i]]))

      matA <- mat_a[[i]]
      matU <- mat_u[[i]]
      matF <- mat_f[[i]]
      matC <- mat_c[[i]]

      mat[[i]] <- list(matA = matA, matU = matU, matF = matF, matC = matC)
    }
  }

  # If U and F provided, make C `NA` or 0 depending on optional matC argument.
  if (!hasArg(mat_a)) {
    mat <- NULL
    for (i in 1:length(mat_u)) {
      if (!hasArg(mat_c)) {
        matC <- matrix(nrow = nrow(mat_u[[i]]), ncol = nrow(mat_u[[i]]))
      }

      matU <- mat_u[[i]]
      matF <- mat_f[[i]]
      if (hasArg(mat_c)) {
        matC <- mat_c[[i]]
      }

      matDims <- c(nrow(matU), nrow(matF), nrow(matC))

      if (!abs(max(matDims) - min(matDims)) < .Machine$double.eps) {
        stop("Dimensions of submatrices U, F and C (if included) must be identical within each set.")
      }

      # Check that dimensions

      matA <- matU + matF + matC

      mat[[i]] <- list(matA = matA, matU = matU, matF = matF, matC = matC)
    }
  }

  # matrixClass (Stage names) ------
  # Where no stage information is given  (1) assume all stages are active, (2)
  # give numeric names for MatrixClassAuthor and MatrixClassNumber.
  if (!hasArg(stages)) {
    # Helper function to create a matrixClass data.frame
    make_matrixClassDataFrame <- function(matrixDimension, ...) {
      if (hasArg(MatrixClassOrganized)) {
        MatrixClassOrganized <- MatrixClassOrganized
      } else {
        MatrixClassOrganized <- rep("active", matrixDimension)
      }
      if (hasArg(MatrixClassAuthor)) {
        MatrixClassAuthor <- MatrixClassAuthor
      } else {
        MatrixClassAuthor <- 1:matrixDimension
      }

      return(data.frame(
        MatrixClassOrganized = MatrixClassOrganized,
        MatrixClassAuthor = MatrixClassAuthor,
        MatrixClassNumber = 1:matrixDimension
      ))
    }

    if (AUFC[1] == TRUE) {
      matrixDimension <- unlist(lapply(X = mat_a, nrow))
    } else {
      matrixDimension <- unlist(lapply(X = mat_u, nrow))
    }

    matrixClassInfo <- lapply(matrixDimension, make_matrixClassDataFrame)
  }

  # Where stage information IS provided: check that the dimensions match and
  # that correct information is provided
  if (hasArg(stages)) {
    if (!inherits(stages, "list")) {
      stop("stages must be provided as a list of data.frame objects")
    }
    for (i in 1:length(stages)) {
      stages[[i]]$MatrixClassNumber <- 1:nrow(stages[[i]])
    }
    matrixClassInfo <- stages
  }
  # metadata ----
  if (!hasArg(metadata)) {
    metadata <- data.frame(
      matrixID = 1:length(mat_a)
    )
  }

  if (hasArg(metadata)) {
    if (nrow(metadata) != length(mat)) {
      stop("The number of rows of metadata does not match the number of matrices")
    }
    if (!"SpeciesAccepted" %in% names(metadata)) {
      warning("Metadata does not include a `SpeciesAccepted` column, so number of species not provided when viewing object.")
    }
  }

  # version ----
  if (hasArg(version)) {
    version <- list(Version = version, DateCreated = Sys.Date())
  }
  if (!hasArg(version)) {
    version <- list(Version = "unknown", DateCreated = Sys.Date())
  }

  # Construct the output CompadreDB object

  out <- list(
    mat = mat,
    matrixClass = matrixClassInfo,
    metadata = metadata,
    version = version
  )

  return(as_cdb(out))
}
