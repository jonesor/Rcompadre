
## Set class union between CompadreMat and CompadreDB, so the same accessor 
## functions can be used for both

################################################################################
#' Accessor methods for CompadreMat and CompadreDB objects
#'
#' Most methods for working with matrices are applicable to both CompadreMat and 
#' CompadreDB objects. These are described on this page (along with a couple) 
#' of methods applicable to only CompadreMat or CompadreDB objects).
#' 
#' @rdname CompadreMatrixMethods

# Set class union for the two classes. Each class 'contains' the class union, 
# so methods set for the union can be used by both classes.
setClassUnion("CompadreMatOrDB", c("CompadreMat", "CompadreDB"))

# matA
#' @rdname CompadreMatrixMethods
#' @export
setGeneric("matA", 
               function(object){
                   standardGeneric("matA")
               }
)
#' The 'matA' function extracts the matA (projection) matrix from a CompadreMat 
#' or CompadreDB object. For CompadreMat objects, this is a single matrix, 
#' for CompadreDB objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matA", signature = "CompadreMatOrDB", 
          function(object){
            if(class(object) %in% "CompadreMat"){
              return(object@matA)
            }
            if(class(object) %in% "CompadreDB"){
              return( lapply(CompadreData(object)$mat, function(M){ M@matA }) )
            }
          }
)


# matU
#' The 'matU' function extracts the matU (survival) matrix from a CompadreMat 
#' or CompadreDB object. For CompadreMat objects, this is a single matrix, 
#' for CompadreDB objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric("matU", 
               function(object){
                   standardGeneric("matU")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matU", signature = "CompadreMatOrDB", 
          function(object){
            if(class(object) %in% "CompadreMat"){
              return(object@matU)
            }
            if(class(object) %in% "CompadreDB"){
              return( lapply(CompadreData(object)$mat, function(M){ M@matU }) )
            }
          }
)

#' The 'matF' function extracts the matF (sexual reproduction) matrix from a CompadreMat 
#' or CompadreDB object. For CompadreMat objects, this is a single matrix, 
#' for CompadreDB objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric("matF", 
               function(object){
                   standardGeneric("matF")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matF", signature = "CompadreMatOrDB", 
          function(object){
            if(class(object) %in% "CompadreMat"){
              return(object@matF)
            }
            if(class(object) %in% "CompadreDB"){
              return( lapply(CompadreData(object)$mat, function(M){ M@matF }) )
            }
          }
)

# matC
#' The 'matC' function extracts the matC (clonal reproduction) matrix from a CompadreMat 
#' or CompadreDB object. For CompadreMat objects, this is a single matrix, 
#' for CompadreDB objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric("matC", 
               function(object){
                   standardGeneric("matC")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matC", signature = "CompadreMatOrDB", 
          function(object){
            if(class(object) %in% "CompadreMat"){
              return(object@matC)
            }
            if(class(object) %in% "CompadreDB"){
              return( lapply(CompadreData(object)$mat, function(M){ M@matC }) )
            }
          }
)

## matrixClass slot

# matrixClass
#' The 'matrixClass' function extracts the matrixClass data frame from a CompadreMat 
#' or CompadreDB object. For CompadreMat objects, this is a single data frame, 
#' for CompadreDB objects this is a list of data frames. The matrixClass data
#' includes information on the matrix, e.g. names of stages.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric("matrixClass", 
               function(object){
                   standardGeneric("matrixClass")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matrixClass", signature = "CompadreMatOrDB", 
          function(object){
            if(class(object) %in% "CompadreMat"){
              return(object@matrixClass)
            }
            if(class(object) %in% "CompadreDB"){
              return( lapply(CompadreData(object)$mat, function(M){ M@matrixClass }) )
            }
          }
)

# stageNames (MatrixClassAuthor)
#' The 'matrixClassAuthor' and 'stageNames' functions extract the matrixClassAuthor column from 
#' the matrixClass data frame from a CompadreMat or CompadreDB object. 
#' For CompadreMat objects, this is a single character vector, for CompadreDB objects 
#' this is a list of character vectors. The matrixClassAuthor data
#' describes the names of the stages as determined by the author of the original 
#' work the matrix was sourced from.
setGeneric("stageNames", 
               function(object){
                   standardGeneric("stageNames")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("stageNames", signature = "CompadreMatOrDB", 
          function(object){
            if(class(object) %in% "CompadreMat"){
              return(object@matrixClass$MatrixClassAuthor)
            }
            if(class(object) %in% "CompadreDB"){
              return( lapply(CompadreData(object)$mat, function(M){ M@matrixClass$matrixClassAuthor }) )
            }
          }
)
setGeneric("MatrixClassAuthor", 
               function(object){
                   standardGeneric("MatrixClassAuthor")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassAuthor", signature = "CompadreMatOrDB", 
          function(object){
            if(class(object) %in% "CompadreMat"){
              return(object@matrixClass$MatrixClassAuthor)
            }
            if(class(object) %in% "CompadreDB"){
              return( lapply(CompadreData(object)$mat, function(M){ M@matrixClass$matrixClassAuthor }) )
            }
          }
)


# stageStatus (matrixClassOrganized)
#' The 'matrixClassAuthor' function extracts the matrixClassAuthor column from 
#' the matrixClass data frame from a CompadreMat or CompadreDB object. 
#' For CompadreMat objects, this is a single character vector, for CompadreDB objects 
#' this is a list of character vectors. The matrixClassAuthor data
#' describes the names of the stages as determined by the author of the original 
#' work the matrix was sourced from.
setGeneric("stageStatus", 
               function(object){
                   standardGeneric("stageStatus")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("stageStatus", signature = "CompadreMatOrDB", 
          function(object){
            if(class(object) %in% "CompadreMat"){
              return(object@matrixClass$matrixClassOrganized)
            }
            if(class(object) %in% "CompadreDB"){
              return( lapply(CompadreData(object)$mat, function(M){ M@matrixClass$matrixClassOrganized }) )
            }
          }
)
setGeneric("MatrixClassOrganized", 
               function(object){
                   standardGeneric("MatrixClassOrganized")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("MatrixClassOrganized", signature = "CompadreMatOrDB", 
          function(object){
            if(class(object) %in% "CompadreMat"){
              return(object@matrixClass$matrixClassOrganized)
            }
            if(class(object) %in% "CompadreDB"){
              return( lapply(CompadreData(object)$mat, function(M){ M@matrixClass$matrixClassOrganized }) )
            }
          }
)
