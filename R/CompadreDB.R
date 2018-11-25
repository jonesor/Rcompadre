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
#' This page describes the CompadreDB class, including methods for accessing the
#' various components of CompadreDB objects.
#' 
#' @name CompadreDB
#' @slot CompadreData A tibble-style data frame with a list-column of matrix
#'   population models (column \code{mat}) and a variety of other metadata
#'   columns.
#' @slot VersionData A list with elements \code{Version} (database version
#'   number), \code{DateCreated} (date of version release), and \code{Agreement}
#'   (a url link to the User Agreement)
setClass("CompadreDB",
         slots = c(
             CompadreData = "data.frame",
             VersionData = "list"
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
    dat <- CompadreData(object)
    errors <- character()
    if (!("mat" %in% names(dat))) {
        msg <- 
"CompadreData must contain a column 'mat' containing matrices\n
 in the form of a list of 'ConmpadreM' objects"
        errors <- c(errors, msg)
    }
    if ("mat" %in% names(dat)) {
        if (!(class(dat$mat) %in% "list")) {
            msg <- "column 'mat' must be a list column"
            errors <- c(errors, msg)
        }
        if (class(dat$mat) %in% "list") {
            if (!(all(sapply(dat$mat, class) %in% "CompadreMat"))) {
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
setAs("list", "CompadreDB", function(from) asCompadreDB(from))



#' Convert legacy COM(P)ADRE database object o CompadreDB
#' @rdname CompadreDB
#' @param from A legacy COM(P)ADRE database
#' @importFrom methods new
#' @importFrom tibble as_tibble add_column
#' @export
asCompadreDB <- function(from) {
    # is all the data required to fill S4 slots there?
    if(!all(c( "metadata", "matrixClass", "mat", "version") %in% names(from))) {
        stop(
"This doesn't appear to be an old compadre data object. It does\n
 not contain all of the components 'metadata', 'mat', 'matrixClass'\n
 and 'version' required to generate a CompadreDB object. See ?CompadreDB")
    }
    # do all the matrix list elements contain matA, matF, matU and matC?
    CheckNames <- function(x) all(names(x) == c("matA", "matU", "matF", "matC"))
    matNamesMatch <- all(vapply(from$mat, CheckNames, logical(1)))
    if(!matNamesMatch) {
        stop(
"Not all matrices in this compadre data object contain all 
 of the components matA, matU, matF and matC (in that order) 
 required to generate CompadreMat objects. See ?CompadreMat"
        )
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
        CompadreData = dat,
        VersionData = db_version)
}



## define a method for showing the object (does not need to be documented)
setMethod("show", signature = (object ="CompadreDB"),
          function (object){
            Mno <- NumberMatrices(object)
            Sno <- NumberAcceptedSpecies(object)
            if(is.character(Version(object))) { V <- paste("Version:",
                                                           Version(object))
                                              }
            if(!is.character(Version(object))) V <- "Unknown"
            #start
            cat(paste("A com(p)adre database ('CompadreDB') object with ",
                      as.character(Sno),
                      " SPECIES and ",
                      as.character(Mno),
                      " MATRICES.\n\n",
                      sep = ""
            ))
            print(CompadreData(object))
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
            return(object@CompadreData)
          }
)

#' @rdname CompadreDB
#' @param x A CompadreDB object
#' @param name The name of a column within x
#' @importFrom methods slotNames
#' @export
setMethod("$", signature = "CompadreDB",
          function(x, name) {
            if (!("CompadreData" %in% slotNames(x))) {
              stop("$ method requires CompadreDB object with slot 'CompadreData'")
            }
            return(CompadreData(x)[[name]])
          }
)

#' @rdname CompadreDB
#' @importFrom methods new slotNames
#' @param value Vector of values to assign to the column
#' @export
setReplaceMethod("$", signature = "CompadreDB", 
                 function(x, name, value) { 
                   if (!("CompadreData" %in% slotNames(x))) {
                     stop("$<- method requires CompadreDB object with slot 'CompadreData'")
                   }
                   if("mat" %in% name) {
                     warning("Replacing 'mat' column may be problematic unless all\nits elements are valid 'CompadreMat' objects.")
                   }
                   datout <- CompadreData(x)
                   datout[[name]] <- value 
                   
                   new("CompadreDB", 
                       CompadreData = datout, 
                       VersionData = VersionData(x))
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
            return(object@VersionData)
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
            return(VersionData(object)$Version)
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
            return(VersionData(object)$DateCreated)
          }
)
