#' Methods for CompadreDB objects
#' 
#' This page describes a variety of methods that can be used with CompadreDB
#' objects, including common data frame operations (\code{head}, \code{names},
#' and \code{merge}), conversion methods (\code{as.data.frame} and
#' \code{as_tibble}), and methods to calculate the number of species
#' (\code{NumberAcceptedSpecies}), studies (\code{NumberStudies}), or matrices
#' (\code{NumberMatrices}).
#' 
#' @name CompadreDB-Methods
NULL



#' @rdname CompadreDB-Methods
#' @param x A CompadreDB object
#' @param ... additional arguments
#' @export
as.data.frame.CompadreDB <- function(x, ...) {
  dat <- x@data
  as.data.frame(dat, ...)
} 

setAs("CompadreDB", "data.frame", function(from)
  as.data.frame.CompadreDB(from))


#' @rdname CompadreDB-Methods
#' @importFrom tibble as_tibble
#' @export
as_tibble.CompadreDB <- function(x) as_tibble(x@data)


#' @rdname CompadreDB-Methods
#' @importFrom utils head
#' @param n The number of rows to extract
#' @export
head.CompadreDB <- function(x, n = 6L, ...) head(x@data, n = n, ...)


#' @rdname CompadreDB-Methods
#' @export
names.CompadreDB <- function(x) {
  names(x@data)
}


#' @rdname CompadreDB-Methods
#' @importFrom tibble as_tibble
#' @importFrom methods new
#' @param y A data.frame to merge with x
#' @export
merge.CompadreDB <- function(x, y, ...) {
  if (inherits(y, "CompadreDB")) {
    y <- y@data
  }
  dataout <- as_tibble(merge(x@data, y, ...))
  
  new("CompadreDB",
      data = dataout, 
      version = x@version)
}


#' The number of accepted binary species names in a CompadreDB object
#' @rdname CompadreDB-Methods
#' @param object A CompadreDB object
#' @export
setGeneric("NumberAcceptedSpecies", 
               function(object){
                   standardGeneric("NumberAcceptedSpecies")
               }
)

#' @rdname CompadreDB-Methods
#' @export
setMethod("NumberAcceptedSpecies", signature = "CompadreDB", 
          function(object){
            return(length(unique(object@data$SpeciesAccepted)))
          }
)


#' The number of different studies in a CompadreDB object
#' @rdname CompadreDB-Methods
#' @export
setGeneric("NumberStudies", 
               function(object){
                   standardGeneric("NumberStudies")
               }
)

#' @rdname CompadreDB-Methods
#' @export
setMethod("NumberStudies", signature = "CompadreDB", 
          function(object){
            return(length(unique(paste0(object@data$Authors,
                                        object@data$Journal,
                                        object@data$YearPublication))))
          }
)


#' The number of projection matrices in a CompadreDB object
#' @rdname CompadreDB-Methods
#' @export
setGeneric("NumberMatrices", 
               function(object){
                   standardGeneric("NumberMatrices")
               }
)

#' @rdname CompadreDB-Methods
#' @export
setMethod("NumberMatrices", signature = "CompadreDB", 
          function(object){
            return(nrow(object@data))
          }
)
