
## Set class union between CompadreM and CompadreData, so the same accessor 
## functions can be used for both

################################################################################
#' Accessor methods for CompadreM and CompadreData objects
#'
#' Most methods for working with matrices are applicable to both CompadreM and 
#' CompadreData objects. These are described on this page (along with a couple) 
#' of methods applicable to only CompadreM or CompadreData objects).
#' 
#' @rdname CompadreMatrixMethods

# Set class union for the two classes. Each class 'contains' the class union, 
# so methods set for the union can be used by both classes.
setClassUnion("CompadreMorData", c("CompadreM", "CompadreData"))

# matA
#' @rdname CompadreMatrixMethods
#' @export
setGeneric("matA", 
               function(object){
                   standardGeneric("matA")
               }
)
#' The 'matA' function extracts the matA (projection) matrix from a CompadreM 
#' or CompadreData object. For CompadreM objects, this is a single matrix, 
#' for CompadreData objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matA", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matA)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@data$mat, function(M){ M@matA }) )
            }
          }
)


# matU
#' The 'matU' function extracts the matU (survival) matrix from a CompadreM 
#' or CompadreData object. For CompadreM objects, this is a single matrix, 
#' for CompadreData objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric("matU", 
               function(object){
                   standardGeneric("matU")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matU", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matU)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@data$mat, function(M){ M@matU }) )
            }
          }
)

#' The 'matF' function extracts the matF (sexual reproduction) matrix from a CompadreM 
#' or CompadreData object. For CompadreM objects, this is a single matrix, 
#' for CompadreData objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric("matF", 
               function(object){
                   standardGeneric("matF")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matF", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matF)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@data$mat, function(M){ M@matF }) )
            }
          }
)

# matC
#' The 'matC' function extracts the matC (clonal reproduction) matrix from a CompadreM 
#' or CompadreData object. For CompadreM objects, this is a single matrix, 
#' for CompadreData objects this is a list of matrices.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric("matC", 
               function(object){
                   standardGeneric("matC")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("matC", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matC)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@data$mat, function(M){ M@matC }) )
            }
          }
)

## matrixClass slot

# matrixClass
#' The 'matrixClass' function extracts the matrixClass data frame from a CompadreM 
#' or CompadreData object. For CompadreM objects, this is a single data frame, 
#' for CompadreData objects this is a list of data frames. The matrixClass data
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
setMethod("matrixClass", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matrixClass)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@data$mat, function(M){ M@matrixClass }) )
            }
          }
)

# stageNames (MatrixClassAuthor)
#' The 'matrixClassAuthor' and 'stageNames' functions extract the matrixClassAuthor column from 
#' the matrixClass data frame from a CompadreM or CompadreData object. 
#' For CompadreM objects, this is a single character vector, for CompadreData objects 
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
setMethod("stageNames", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matrixClass$MatrixClassAuthor)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@data$mat, function(M){ M@matrixClass$matrixClassAuthor }) )
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
setMethod("MatrixClassAuthor", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matrixClass$MatrixClassAuthor)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@data$mat, function(M){ M@matrixClass$matrixClassAuthor }) )
            }
          }
)


# stageStatus (matrixClassOrganized)
#' The 'matrixClassAuthor' function extracts the matrixClassAuthor column from 
#' the matrixClass data frame from a CompadreM or CompadreData object. 
#' For CompadreM objects, this is a single character vector, for CompadreData objects 
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
setMethod("stageStatus", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matrixClass$matrixClassOrganized)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@data$mat, function(M){ M@matrixClass$matrixClassOrganized }) )
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
setMethod("MatrixClassOrganized", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matrixClass$matrixClassOrganized)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@data$mat, function(M){ M@matrixClass$matrixClassOrganized }) )
            }
          }
)
