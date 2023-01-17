#' Accessor methods for CompadreMat and CompadreDB objects
#'
#' Most methods for working with matrices are applicable to both CompadreMat and
#' CompadreDB objects. These are described on this page (along with a couple)
#' of methods applicable to only CompadreMat or CompadreDB objects).
#'
#' @name CompadreMatrixMethods
NULL



# matA
#' @rdname CompadreMatrixMethods
#' @param object A CompadreDB object
#' @export
setGeneric(
  "matA",
  function(object) {
    standardGeneric("matA")
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matA",
  signature = "CompadreMat",
  function(object) {
    rownames(object@matA) <- colnames(object@matA)
    return(object@matA)
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matA",
  signature = "CompadreDB",
  function(object) {
    return(lapply(object@data$mat, function(M) {
      rownames(M@matA) <- colnames(M@matA)
      return(M@matA)
    }))
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matA",
  signature = "list",
  function(object) {
    if (!all(vapply(object, class, "") == "CompadreMat")) {
      stop(
        "All elements of list passed to matA() must be of class ",
        "CompadreMat"
      )
    }
    # return(lapply(object, function(M) { M@matA }))
    return(lapply(object, function(M) {
      rownames(M@matA) <- colnames(M@matA)
      return(M@matA)
    }))
  }
)


# matU
#' The 'matU' function extracts the matU (survival) matrix from a CompadreMat
#' or CompadreDB object. For CompadreMat objects, this is a single matrix,
#' for CompadreDB objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric(
  "matU",
  function(object) {
    standardGeneric("matU")
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matU",
  signature = "CompadreMat",
  function(object) {
    rownames(object@matU) <- colnames(object@matU)
    return(object@matU)
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matU",
  signature = "CompadreDB",
  function(object) {
    return(lapply(object@data$mat, function(M) {
      rownames(M@matU) <- colnames(M@matU)
      return(M@matU)
    }))
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matU",
  signature = "list",
  function(object) {
    if (!all(vapply(object, class, "") == "CompadreMat")) {
      stop(
        "All elements of list passed to matU() must be of class ",
        "CompadreMat"
      )
    }
    # return(lapply(object, function(M) { M@matU }))
    return(lapply(object, function(M) {
      rownames(M@matU) <- colnames(M@matU)
      return(M@matU)
    }))
  }
)




# matF
#' The 'matF' function extracts the matF (sexual reproduction) matrix from a
#' CompadreMat or CompadreDB object. For CompadreMat objects, this is a single
#' matrix, for CompadreDB objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric(
  "matF",
  function(object) {
    standardGeneric("matF")
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matF",
  signature = "CompadreMat",
  function(object) {
    rownames(object@matF) <- colnames(object@matF)
    return(object@matF)
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matF",
  signature = "CompadreDB",
  function(object) {
    return(lapply(object@data$mat, function(M) {
      rownames(M@matF) <- colnames(M@matF)
      return(M@matF)
    }))
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matF",
  signature = "list",
  function(object) {
    if (!all(vapply(object, class, "") == "CompadreMat")) {
      stop(
        "All elements of list passed to matF() must be of class ",
        "CompadreMat"
      )
    }
    # return(lapply(object, function(M) { M@matF }))
    return(lapply(object, function(M) {
      rownames(M@matF) <- colnames(M@matF)
      return(M@matF)
    }))
  }
)




# matC
#' The 'matC' function extracts the matC (clonal reproduction) matrix from a
#' CompadreMat or CompadreDB object. For CompadreMat objects, this is a single
#' matrix, for CompadreDB objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric(
  "matC",
  function(object) {
    standardGeneric("matC")
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matC",
  signature = "CompadreMat",
  function(object) {
    rownames(object@matC) <- colnames(object@matC)
    return(object@matC)
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matC",
  signature = "CompadreDB",
  function(object) {
    return(lapply(object@data$mat, function(M) {
      rownames(M@matC) <- colnames(M@matC)
      return(M@matC)
    }))
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matC",
  signature = "list",
  function(object) {
    if (!all(vapply(object, class, "") == "CompadreMat")) {
      stop(
        "All elements of list passed to matC() must be of class ",
        "CompadreMat"
      )
    }
    # return(lapply(object, function(M) { M@matC }))
    return(lapply(object, function(M) {
      rownames(M@matC) <- colnames(M@matC)
      return(M@matC)
    }))
  }
)





## matrixClass slot

# matrixClass
#' The 'matrixClass' function extracts the matrixClass data frame from a
#' CompadreMat or CompadreDB object. For CompadreMat objects, this is a single
#' data frame, for CompadreDB objects this is a list of data frames. The
#' matrixClass data includes information on the matrix, e.g. names of stages.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric(
  "matrixClass",
  function(object) {
    standardGeneric("matrixClass")
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matrixClass",
  signature = "CompadreMat",
  function(object) {
    return(object@matrixClass)
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matrixClass",
  signature = "CompadreDB",
  function(object) {
    return(lapply(object@data$mat, function(M) {
      M@matrixClass
    }))
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matrixClass",
  signature = "list",
  function(object) {
    if (!all(vapply(object, class, "") == "CompadreMat")) {
      stop(
        "All elements of list passed to matrixClass() must be of ",
        "class CompadreMat"
      )
    }
    return(lapply(object, function(M) {
      M@matrixClass
    }))
  }
)




# MatrixClassAuthor
#' The 'MatrixClassAuthor' function extracts the MatrixClassAuthor column from
#' the matrixClass data frame from a CompadreMat or CompadreDB object. For
#' CompadreMat objects, this is a single character vector, for CompadreDB
#' objects this is a list of character vectors. The matrixClassAuthor data
#' describes the names of the stages as determined by the author of the original
#' work the matrix was sourced from.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric(
  "MatrixClassAuthor",
  function(object) {
    standardGeneric("MatrixClassAuthor")
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassAuthor",
  signature = "CompadreMat",
  function(object) {
    return(object@matrixClass$MatrixClassAuthor)
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassAuthor",
  signature = "CompadreDB",
  function(object) {
    return(lapply(
      object@data$mat,
      function(M) {
        M@matrixClass$MatrixClassAuthor
      }
    ))
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassAuthor",
  signature = "list",
  function(object) {
    if (!all(vapply(object, class, "") == "CompadreMat")) {
      stop(
        "All elements of list passed to MatrixClassAuthor() must ",
        "be of class CompadreMat"
      )
    }
    return(lapply(
      object,
      function(M) {
        M@matrixClass$MatrixClassAuthor
      }
    ))
  }
)


# MatrixClassOrganized
#' The 'MatrixClassOrganized' function extracts the MatrixClassOrganized column
#' from the matrixClass data frame from a CompadreMat or CompadreDB object. For
#' CompadreMat objects, this is a single character vector, for CompadreDB
#' objects this is a list of character vectors. The matrixClassAuthor data
#' describes the names of the stages as determined by the author of the original
#' work the matrix was sourced from.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric(
  "MatrixClassOrganized",
  function(object) {
    standardGeneric("MatrixClassOrganized")
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassOrganized",
  signature = "CompadreMat",
  function(object) {
    return(object@matrixClass$MatrixClassOrganized)
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassOrganized",
  signature = "CompadreDB",
  function(object) {
    return(lapply(
      object@data$mat,
      function(M) {
        M@matrixClass$MatrixClassOrganized
      }
    ))
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassOrganized",
  signature = "list",
  function(object) {
    if (!all(vapply(object, class, "") == "CompadreMat")) {
      stop(
        "All elements of list passed to MatrixClassOrganized() ",
        "must be of class CompadreMat"
      )
    }
    return(lapply(
      object,
      function(M) {
        M@matrixClass$MatrixClassOrganized
      }
    ))
  }
)




# MatrixClassNumber
#' @rdname CompadreMatrixMethods
#' @export
setGeneric(
  "MatrixClassNumber",
  function(object) {
    standardGeneric("MatrixClassNumber")
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassNumber",
  signature = "CompadreMat",
  function(object) {
    return(object@matrixClass$MatrixClassNumber)
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassNumber",
  signature = "CompadreDB",
  function(object) {
    return(lapply(
      object@data$mat,
      function(M) {
        M@matrixClass$MatrixClassNumber
      }
    ))
  }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassNumber",
  signature = "list",
  function(object) {
    if (!all(vapply(object, class, "") == "CompadreMat")) {
      stop(
        "All elements of list passed to MatrixClassNumber() ",
        "must be of class CompadreMat"
      )
    }
    return(lapply(
      object,
      function(M) {
        M@matrixClass$MatrixClassNumber
      }
    ))
  }
)
