
## Set class union between CompadreM and CompadreData, so the same accessor 
## functions can be used for both

################################################################################
#' Accessor methods for CompadreM and CompadreData objects
#'
#' This page describes methods for both the CompadreM and CompadreData classes. 
#' In many cases the same method can be used for both classes, but some methods
#' are only valid for one class.
#' 
#' @rdname CompadreUnionMethods

# Set class union for the two classes. Each class 'contains' the class union, 
# so methods set for the union can be used by both classes.
setClassUnion("CompadreMorData", c("CompadreM", "CompadreData"))

# matA
#' @rdname CompadreUnionMethods
#' @export
setGeneric("matA", 
               function(object){
                   standardGeneric("matA")
               }
)
#' @rdname CompadreUnionMethods
#' @export
setMethod("matA", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matA)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@mat, function(M){ M@matA }) )
            }
          }
)

# matU
#' @rdname CompadreUnionMethods
#' @export
setGeneric("matU", 
               function(object){
                   standardGeneric("matU")
               }
)
#' @rdname CompadreUnionMethods
#' @export
setMethod("matU", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matU)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@mat, function(M){ M@matU }) )
            }
          }
)

# matF
#' @rdname CompadreUnionMethods
#' @export
setGeneric("matF", 
               function(object){
                   standardGeneric("matF")
               }
)
#' @rdname CompadreUnionMethods
#' @export
setMethod("matF", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matF)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@mat, function(M){ M@matF }) )
            }
          }
)

# matC
#' @rdname CompadreUnionMethods
#' @export
setGeneric("matC", 
               function(object){
                   standardGeneric("matC")
               }
)
#' @rdname CompadreUnionMethods
#' @export
setMethod("matC", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matC)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@mat, function(M){ M@matC }) )
            }
          }
)

## matrixClass slot

# matrixClass
#' @rdname CompadreUnionMethods
#' @export
setGeneric("matrixClass", 
               function(object){
                   standardGeneric("matrixClass")
               }
)
#' @rdname CompadreUnionMethods
#' @export
setMethod("matrixClass", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matrixClass)
            }
            if(class(object) %in% "CompadreData"){
              return( lapply(object@mat, function(M){ M@matrixClass }) )
            }
          }
)

# stageNames (MatrixClassAuthor)
setGeneric("stageNames", 
               function(object){
                   standardGeneric("stageNames")
               }
)
#' @rdname CompadreUnionMethods
#' @export
setMethod("stageNames", signature = "CompadreMorData", 
          function(object){
            if(class(object) %in% "CompadreM"){
              return(object@matrixClass$MatrixClassAuthor)
            }
            if(class(object) %in% "CompadreData"){
              return( sapply(object@mat, function(M){ M@matrixClass$matrixClassAuthor }) )
            }
          }
)

