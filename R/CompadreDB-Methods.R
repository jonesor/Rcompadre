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
  dat <- CompadreData(x)
  as.data.frame(dat, ...)
} 

setAs("CompadreDB", "data.frame", function(from)
  as.data.frame.CompadreDB(from))


#' @rdname CompadreDB-Methods
#' @importFrom tibble as_tibble
#' @export
as_tibble.CompadreDB <- function(x) as_tibble(CompadreData(x))


#' @rdname CompadreDB-Methods
#' @importFrom utils head
#' @param n The number of rows to extract
#' @export
head.CompadreDB <- function(x, n = 6L, ...) head(CompadreData(x), n = n, ...)


#' @rdname CompadreDB-Methods
#' @export
names.CompadreDB <- function(x) {
  if (!("CompadreData" %in% slotNames(x))) {
    stop("names method requires CompadreData object with slot 'CompadreData'")
  }
  names(CompadreData(x))
}


#' @rdname CompadreDB-Methods
#' @importFrom tibble as_tibble
#' @param y A data.frame to merge with x
#' @export
merge.CompadreDB <- function(x, y, ...) {
  if (inherits(y, "CompadreDB")) {
    y <- CompadreData(y)
  }
  dataout <- as_tibble(merge(CompadreData(x), y, ...))
  dbout <- methods::new("CompadreDB", CompadreData = dataout, 
                                      VersionData = VersionData(x))
  dbout
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
            return(length(unique(object$SpeciesAccepted)))
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
            return(length(unique(paste0(object$Authors,
                                        object$Journal,
                                        object$YearPublication
                                        ))))
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
            return(dim(CompadreData(object))[1])
          }
)
