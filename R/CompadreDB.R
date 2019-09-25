# Class CompadreDB, definition and methods

# Copyright (c) 2017 Tamora D. James and Iain M. Stott

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.




#' CompadreDB Class
#' 
#' @description 
#' This page describes the CompadreDB class, including methods for accessing the
#' slots (see functions \code{CompadreData} and \code{VersionData}), accessing
#' (\code{$}) and replacing (\code{$<-}) columns within the \code{data} slot,
#' accessing elements from the \code{version} slot (see functions
#' \code{VersionData} and \code{DateCreated}), and converting legacy database
#' objects to the CompadreDB class (see \code{as_cdb}).
#' 
#' @name CompadreDB
#' 
#' @slot data A tibble-style data frame with a list-column of matrix population
#'   models (column \code{mat}) and a variety of other metadata columns.
#' @slot version A list with elements \code{Version} (database version number),
#'   \code{DateCreated} (date of version release), and \code{Agreement} (a url
#'   link to the User Agreement)
#' 
#' @seealso 
#' \code{\link{CompadreDB-Methods}} \code{\link{CompadreDB-Subsetting}}
#' 
#' @author Iain M. Stott
#' @author Tamora D. James
#' 
#' @examples
#' # extract entire 'data' slot
#' dat <- CompadreData(Compadre)
#' 
#' # access the date of database creation
#' DateCreated(Compadre)
#' 
#' # extract column SpeciesAccepted
#' Compadre$SpeciesAccepted
#' 
#' # create new list-column with stage-specific survival
#' Compadre$stage_survival <- lapply(Compadre$mat, function(x) colSums(x@matU))
#' 
setClass("CompadreDB",
         slots = c(
             data = "data.frame",
             version = "list"
             )
         )



## define a method for initialize (does not need to be documented)
#' @importFrom methods callNextMethod validObject
setMethod("initialize", "CompadreDB",
    function(.Object, ...) {
        .Object <- methods::callNextMethod()
        methods::validObject(.Object)
        .Object
    })



## define validity check function (does not need to be documented)
validCompadreDB <- function(object) {
    dat <- object@data
    errors <- character()
    if (!("mat" %in% names(dat))) {
        msg <- 
"CompadreDB must contain a column 'mat' containing matrices\n
 in the form of a list of 'ConmpadreMat' objects"
        errors <- c(errors, msg)
    }
    if ("mat" %in% names(dat)) {
        if (!(class(dat$mat) %in% "list")) {
            msg <- "column 'mat' must be a list column"
            errors <- c(errors, msg)
        }
        if (class(dat$mat) %in% "list") {
            if (!(all(vapply(dat$mat, class, "") %in% "CompadreMat"))) {
                msg <- "all elements of 'mat' must be 'CompadreMat' objects"
                errors <- c(errors, msg)
            }
        }
    }
    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }
}
setValidity("CompadreDB", validCompadreDB)



## define method to coerce old compadre db object to CompadreDB class
setAs("list", "CompadreDB", function(from) as_cdb(from))


#' Convert legacy COM(P)ADRE database object to CompadreDB
#' 
#' Convert a legacy COM(P)ADRE database object (of class 'list') to a CompadreDB
#' object.
#' 
#' @param from A legacy COM(P)ADRE database
#' 
#' @return A CompadreDB object
#' 
#' @author Iain M. Stott
#' 
#' @examples
#' Compadre <- as_cdb(CompadreLegacy)
#' 
#' @importFrom methods new
#' @importFrom tibble as_tibble add_column
#' @export
as_cdb <- function(from) {
  # is all the data required to fill S4 slots there?
  if(!all(c("metadata", "matrixClass", "mat", "version") %in% names(from))) {
    stop("This doesn't appear to be an old compadre data object. It does ",
         "not contain all of the components 'metadata', 'mat', ",
         "'matrixClass' and 'version' required to generate a CompadreDB ",
         "object. See ?CompadreDB", call. = FALSE)
  }
  # do all the matrix list elements contain matA, matF, matU and matC?
  CheckNames <- function(x) {
    all(c("matA", "matU", "matF", "matC") %in% names(x))
  }
  matNamesMatch <- all(vapply(from$mat, CheckNames, logical(1)))
  if(!matNamesMatch) {
    stop("Not all matrices in this compadre data object contain all of ",
         "the components matA, matU, matF and matC required to generate ",
         "CompadreMat objects. See ?CompadreMat", call. = FALSE)
  }
  # get matrices and coerce into CompadreMat objects
  mat <- lapply(seq_along(from$mat), function(i) {
    new("CompadreMat",
        matA = from$mat[[i]]$matA,
        matU = from$mat[[i]]$matU,
        matF = from$mat[[i]]$matF,
        matC = from$mat[[i]]$matC,
        matrixClass = as.data.frame(from$matrixClass[[i]]))
  })
  
  # add matrices to metadata as a list column
  dat <- as_tibble(from$metadata)
  dat <- add_column(dat, mat = mat, .before = 1)
  
  # strip out species/study/matrix counts from legacy db, if any
  db_version <- from$version
  db_version$NumberAcceptedSpecies <- NULL
  db_version$NumberStudies <- NULL
  db_version$NumberMatrices <- NULL
  
  # create a new CompadreDB object with the new data and version
  new("CompadreDB",
      data = dat,
      version = db_version)
}



## define a method for showing the object (does not need to be documented)
setMethod("show", signature = (object ="CompadreDB"),
          function (object) {
            Mno <- NumberMatrices(object)
            Sno <- ifelse("SpeciesAccepted" %in% names(object@data),
                          NumberAcceptedSpecies(object),
                          "??")
            V <- ifelse(is.character(Version(object)),
                        paste("Version:", Version(object)),
                        "Version: Unknown")
            #start
            cat(paste("A COM(P)ADRE database ('CompadreDB') object with ",
                      as.character(Sno),
                      " SPECIES and ",
                      as.character(Mno),
                      " MATRICES.\n\n",
                      sep = ""
            ))
            print(object@data)
          }
)



## Accessors -------------------------------------------------------------------

#' Extract the data slot
#' @rdname CompadreDB
#' @param object A CompadreDB object
#' @export
setGeneric("CompadreData", 
           function(object){
             standardGeneric("CompadreData")
           }
)

#' @rdname CompadreDB
#' @export
setMethod("CompadreData", signature = "CompadreDB", 
          function(object){
            return(object@data)
          }
)

#' @rdname CompadreDB
#' @param x A CompadreDB object
#' @param name The name of a column within x
#' @export
setMethod("$", signature = "CompadreDB",
          function(x, name) {
            return(x@data[[name]])
          }
)

#' @rdname CompadreDB
#' @importFrom methods new
#' @param value Vector of values to assign to the column
#' @export
setReplaceMethod("$", signature = "CompadreDB", 
                 function(x, name, value) { 
                   datout <- x@data
                   datout[[name]] <- value 
                   
                   new("CompadreDB", 
                       data = datout, 
                       version = x@version)
                 }
)

#' @rdname CompadreDB
#' @param i,j elements to extract or replace (see \link{[[.data.frame})
#' @param ... ignored
#' @export
setMethod("[[", c("CompadreDB", "ANY", "missing"), 
          function(x, i, j, ...) {
            x@data[[i]]
          }
)


#' @rdname CompadreDB
#' @importFrom methods new
#' @export
setReplaceMethod("[[", c("CompadreDB", "ANY", "missing", "ANY"), 
                 function(x, i, j, value) {
                   dat <- x@data
                   dat[[i]] <- value 
                   
                   new("CompadreDB", 
                       data = dat, 
                       version = x@version)
                 }
)


#' Extract the version slot
#' @rdname CompadreDB
#' @export
setGeneric("VersionData", 
           function(object){
             standardGeneric("VersionData")
           }
)

#' @rdname CompadreDB
#' @export
setMethod("VersionData", signature = "CompadreDB", 
          function(object){
            return(object@version)
          }
)

#' The CompadreDB version number
#' @rdname CompadreDB
#' @export
setGeneric("Version", 
           function(object){
             standardGeneric("Version")
           }
)

#' @rdname CompadreDB
#' @export
setMethod("Version", signature = "CompadreDB", 
          function(object){
            return(object@version$Version)
          }
)

#' The date that a CompadreDB version was created
#' @rdname CompadreDB
#' @export
setGeneric("DateCreated", 
           function(object){
             standardGeneric("DateCreated")
           }
)

#' @rdname CompadreDB
#' @export
setMethod("DateCreated", signature = "CompadreDB", 
          function(object){
            return(object@version$DateCreated)
          }
)
