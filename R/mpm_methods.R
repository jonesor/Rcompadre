#' Extract stage-class information from CompadreMat or CompadreDB objects
#' 
#' @description
#' Methods for extracting stage-class information from CompadreMat or CompadreDB
#' objects, including whether the matrix population model includes one or more
#' propagule stages (\code{mpm_has_prop}), dormant stages (\code{mpm_has_dorm}),
#' or active stages (\code{mpm_has_active}), and the integer index of the first
#' active stage class (\code{mpm_first_active}).
#' 
#' These methods will return a single value if passed a CompadreMat object, or a
#' vector of values if passed a CompadreDB object (one value for every
#' CompadreMat object within the column 'mat').
#' 
#' @return No return value, called for side effects
#' 
#' @name mpm_methods
#' 
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @family data checking
#' 
#' @examples 
#' # with CompadreMat object
#' mpm_has_prop(Compadre$mat[[1]])
#' mpm_has_active(Compadre$mat[[1]])
#' mpm_has_dorm(Compadre$mat[[1]])
#' mpm_first_active(Compadre$mat[[1]])
#' 
#' # with CompadreDB object
#' mpm_has_prop(Compadre)
#' mpm_has_active(Compadre)
#' mpm_has_dorm(Compadre)
#' mpm_first_active(Compadre)
#' 
NULL




#' @rdname mpm_methods
#' @param object A CompadreMat or CompadreDB object
#' @export
setGeneric("mpm_has_prop", 
           function(object) {
             standardGeneric("mpm_has_prop")
           }
)
#' @rdname mpm_methods
#' @export
setMethod("mpm_has_prop", signature = "CompadreMat", 
          function(object) {
            "prop" %in% object@matrixClass$MatrixClassOrganized
          }
)
#' @rdname mpm_methods
#' @export
setMethod("mpm_has_prop", signature = "CompadreDB", 
          function(object) {
            vapply(object@data$mat,
                   function(m) "prop" %in% m@matrixClass$MatrixClassOrganized,
                   logical(1))
          }
)


#' @rdname mpm_methods
#' @export
setGeneric("mpm_has_active", 
           function(object) {
             standardGeneric("mpm_has_active")
           }
)
#' @rdname mpm_methods
#' @export
setMethod("mpm_has_active", signature = "CompadreMat", 
          function(object) {
            "active" %in% object@matrixClass$MatrixClassOrganized
          }
)
#' @rdname mpm_methods
#' @export
setMethod("mpm_has_active", signature = "CompadreDB", 
          function(object) {
            vapply(object@data$mat,
                   function(m) "active" %in% m@matrixClass$MatrixClassOrganized,
                   logical(1))
          }
)


#' @rdname mpm_methods
#' @export
setGeneric("mpm_has_dorm", 
           function(object) {
             standardGeneric("mpm_has_dorm")
           }
)
#' @rdname mpm_methods
#' @export
setMethod("mpm_has_dorm", signature = "CompadreMat", 
          function(object) {
            "dorm" %in% object@matrixClass$MatrixClassOrganized
          }
)
#' @rdname mpm_methods
#' @export
setMethod("mpm_has_dorm", signature = "CompadreDB", 
          function(object) {
            vapply(object@data$mat,
                   function(m) "dorm" %in% m@matrixClass$MatrixClassOrganized,
                   logical(1))
          }
)


#' @rdname mpm_methods
#' @export
setGeneric("mpm_first_active", 
           function(object) {
             standardGeneric("mpm_first_active")
           }
)
#' @rdname mpm_methods
#' @export
setMethod("mpm_first_active", signature = "CompadreMat", 
          function(object) {
            mclass <- object@matrixClass$MatrixClassOrganized
            ifelse(!"active" %in% mclass,
                   NA_integer_,
                   min(which(mclass == "active")))
          }
)
#' @rdname mpm_methods
#' @export
setMethod("mpm_first_active", signature = "CompadreDB", 
          function(object) {
            vapply(object@data$mat,
                   function(m) {
                     mclass <- m@matrixClass$MatrixClassOrganized
                     ifelse(!"active" %in% mclass,
                            NA_integer_,
                            min(which(mclass == "active")))
                   },
                   integer(1))
          }
)

