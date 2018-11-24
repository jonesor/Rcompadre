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

################################################################################
#' CompadreDB class
#' 
#' This page describes methods for accessing any metadata information from 
#' CompadreDB objects.
#' 
#' @name CompadreDBMethods

setClass("CompadreDB",
         slots = c(
             CompadreData = "data.frame",
             VersionData = "list"
             )
         )



################################################################################
## Initialize & check

## define a method for initialize (does not need to be documented)
#' @importFrom methods callNextMethod validObject
setMethod("initialize", "CompadreDB",
    function(.Object, ...) {
        .Object <- methods::callNextMethod()
        methods::validObject(.Object)
        .Object
    })


## -----------------------------------------------------------------------------
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



################################################################################
## Working with all data (matrices and metadata)

## -----------------------------------------------------------------------------
## define method to coerce old compadre db object to CompadreDB class
setAs("list", "CompadreDB", function(from) asCompadreDB(from))

#' This page describes methods for working with the entire database (including 
#' both matrices and metadata) using CompadreDB objects.
#' @rdname CompadreDBMethods
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
        methods::new("CompadreMat",
                    matA = from$mat[[i]]$matA,
                    matU = from$mat[[i]]$matU,
                    matF = from$mat[[i]]$matF,
                    matC = from$mat[[i]]$matC,
                    matrixClass = as.data.frame(from$matrixClass[[i]]))
    })
    
    # add matrices to metadata as a list column
    dat <- as_tibble(from$metadata)
    dat <- add_column(dat, mat = mat, .before = 1)
    
    # create a new CompadreDB object with the new data and version
    new("CompadreDB",
        CompadreData = dat,
        VersionData = from$version)
}


## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
# Subset, replace, merge

#' This method enables subsetting the data using square brackets, i.e. for a
#' CompadreDB object "DB" one can use DB[1:2, 1:10] to get a new CompadreDB
#' object that includes the first two matrices and the the first 10 columns of 
#' their associated metadata. It's possible to pass logical vectors, numeric
#' vectors, or character vectors that match the row or column names of the 
#' metadata. 
#' @rdname CompadreDBMethods
#' @importFrom methods new
#' @export
setMethod(f = "[", signature = signature(x = "CompadreDB", i = "ANY", j = "ANY", drop = "ANY"), 
          function(x, i, j, ..., drop = FALSE) {
            dat <- CompadreData(x)
            if(!missing(i)){
              if(!any(is.logical(i), is.numeric(i), is.character(i))) {
                stop("subset criteria must be logical, numeric (column / row numbers)\nor character (column / row names)")
              }
            }
            if(!missing(j)){
              if(!any(is.logical(j), is.numeric(j), is.character(j))) {
                stop("subset criteria must be logical, numeric (column / row numbers)\nor character (column / row names)")
              }
              mat_col <- which(names(dat) == "mat")
              # test for length 1 b/c in [.data.frame, x[,TRUE] selects all columns
              if(is.logical(j) & j[mat_col] != TRUE & length(j) != 1){
                warning("'mat' was included in the output by default, although not selected")
                j[mat_col] <- TRUE
              }
              if(is.numeric(j) & !(mat_col %in% j)){
                warning("'mat' was included in the output by default, although not selected")
                j <- c(mat_col, j)
              }
              if(is.character(j) & !("mat" %in% j)){
                warning("'mat' was included in the output by default, although not selected")
                j <- c("mat", j)
              }
            }
            
            new("CompadreDB",
                CompadreData = dat[i, j, drop = FALSE],
                VersionData = VersionData(x))
          }
)


#' @name CompadreDataMethods
#' @export
as.data.frame.CompadreDB <- function(x) as.data.frame(CompadreData(x))

#' @name CompadreDataMethods
#' @importFrom tibble as_tibble
#' @export
as_tibble.CompadreDB <- function(x) as_tibble(CompadreData(x))

#' @name CompadreDataMethods
#' @importFrom utils head
#' @export
head.CompadreDB <- function(x) head(CompadreData(x))

#' @name CompadreDataMethods
#' @importFrom tibble as_tibble
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


#' @name CompadreDataMethods
#' @export
subset.CompadreDB <- function(x, subset, select, drop = FALSE, ...) {
  
  if (!"data" %in% slotNames(x)) {
    stop("subset method requires CompadreDB object with slot 'data'")
  }
  
  if (missing(subset)) {
    r <- rep_len(TRUE, nrow(x@data))
  } else {
    e <- substitute(subset)
    r <- eval(e, x@data, parent.frame())
    if (!is.logical(r)) stop("'subset' must be logical")
    r <- r & !is.na(r)
  }
  
  if (missing(select)) {
    vars <- TRUE
  } else {
    nl <- as.list(seq_along(x@data))
    names(nl) <- names(x@data)
    vars <- eval(substitute(select), nl, parent.frame())
  }
  # dat <- data(x)[i, j, drop = FALSE]
  x[r, vars, drop = drop]
}


################################################################################
## Working with individual variables

# CompadreData
#' The CompadreData accessor function accesses the data of a CompadreDB
#' object, which is a data frame including the matrices and all the 
#' metadata variables. Further methods described below allow the user to access 
#' individual metadata variables as vectors.
#' @name CompadreDataMethods
#' @export
setGeneric("CompadreData", 
               function(object){
                   standardGeneric("CompadreData")
               }
)
#' @rdname CompadreDataMethods
#' @export
setMethod("CompadreData", signature = "CompadreDB", 
          function(object){
            return(object@CompadreData)
          }
)

# Metadata variables
#' These functions access certain variables from the metadata slot of a
#' CompadreDB object. These methods are implemented using 
#' CompadreDBObject$VariableName and are effectively the same as using 
#' `CompadreData(CompadreDBObject, "VariableName")`. Most variables are available 
#' for both compadre and comadre, but some are only available for either one or 
#' the other. Where a variable is not available for the chosen database, the 
#' function will simply return an error. The variables are: 
#' "SpeciesAuthor", "SpeciesAccepted", "CommonName", "Infraspecific" [comadre], 
#' "Genus" [compadre] or "GenusAccepted" [comadre], "GenusAuthor" [comadre],
#' "Family", "Order" , "Class", "Phylum", "Kingdom", 
#' "OrganismType", "DicotMonoc", [compadre], "AngioGymno" [compadre],
#' "Authors", "Journal", "YearPublication", "DOI.ISBN", "AdditionalSource",
#' "StudyDuration", "StudyStart", "StudyEnd", "AnnualPeriodicity",
#' "NumberPopulations", "MatrixCriteriaSize", "MatrixCriteriaOntogeny",
#' "MatrixCriteriaAge", "MatrixPopulation", 
#' "Lat", "Lon", "Altitude", "Country", "Continent", "Ecoregion",
#' "StudiedSex",
#' "MatrixComposite", "MatrixTreatment", "MatrixCaptivity",
#' "MatrixStartYear", "MatrixStartSeason", "MatrixStartMonth",
#' "MatrixEndYear", "MatrixEndSeason", "MatrixEndMonth",
#' "MatrixSplit", "MatrixFec", "Observation",
#' "MatrixDimension", "SurvivalIssue".
#' 
#' @rdname CompadreDataMethods
#' @export
setMethod("$", signature = "CompadreDB",
          function(x, name) {
            if (!("CompadreData" %in% slotNames(x))) {
              stop("$ method requires CompadreDB object with slot 'CompadreData'")
            }
            return(CompadreData(x)[[name]])
          }
)


#' @rdname CompadreDataMethods
#' @export
setReplaceMethod("$", signature = "CompadreDB", 
                 function(x, name, value) { 
                   if (!("CompadreData" %in% slotNames(x))) {
                     stop("$<- method requires CompadreDB object with slot 'CompadreData'")
                   }
                   if("mat" %in% var) {
                       warning("Replacing 'mat' column may be problematic unless all\nits elements are valid 'CompadreMat' objects.")
                   }
                   datout <- CompadreData(x)
                   datout[[var]] <- value 
                   dbout <- methods::new("CompadreDB", 
                                         CompadreData = datout, 
                                         VersionData = VersionData(x))
                   return(x)
                 }
)


#' @rdname CompadreDataMethods
#' @export
names.CompadreDB <- function(x) {
  if (!("CompadreData" %in% slotNames(x))) {
    stop("names method requires CompadreData object with slot 'CompadreData'")
  }
  names(CompadreData(x))
}



## -----------------------------------------------------------------------------
## version slot

################################################################################
## Working with version information

# All version data
#' All version information (including subset information) for a CompadreDB 
#' object, as a list.
#' @rdname CompadreDataMethods
#' @export
setGeneric("VersionData", 
               function(object, ...){
                   standardGeneric("VersionData")
               }
)
#' @rdname CompadreDataMethods
#' @export
setMethod("VersionData", signature = "CompadreDB", 
          function(object){
            return(object@VersionData)
          }
)

# Version
#' The version (including subset if relevant) of a CompadreDB object.
#' @rdname CompadreDataMethods
#' @export
setGeneric("Version", 
               function(object, ...){
                   standardGeneric("Version")
               }
)
#' @rdname CompadreDataMethods
#' @export
setMethod("Version", signature = "CompadreDB", 
          function(object){
            return(VersionData(object)$Version)
          }
)

# DateCreated
#' The date a CompadreDB Version was created.
#' @rdname DateCreated
#' @export
setGeneric("DateCreated", 
               function(object, ...){
                   standardGeneric("DateCreated")
               }
)
#' @rdname CompadreDataMethods
#' @export
setMethod("DateCreated", signature = "CompadreDB", 
          function(object){
            return(VersionData(object)$DateCreated)
          }
)

# NumberAcceptedSpecies
#' The number of accepted binary species names in a CompadreDB object.
#' @rdname CompadreDataMethods
#' @export
setGeneric("NumberAcceptedSpecies", 
               function(object, ...){
                   standardGeneric("NumberAcceptedSpecies")
               }
)
#' @rdname CompadreDataMethods
#' @export
setMethod("NumberAcceptedSpecies", signature = "CompadreDB", 
          function(object){
            return(length(unique(object$SpeciesAccepted)))
          }
)

# NumberStudies
#' The number of different studies in a CompadreDB object.
#' @rdname CompadreDataMethods
#' @export
setGeneric("NumberStudies", 
               function(object, ...){
                   standardGeneric("NumberStudies")
               }
)
#' @rdname CompadreDataMethods
#' @export
setMethod("NumberStudies", signature = "CompadreDB", 
          function(object){
            return(length(unique(paste0(object$Authors,
                                        object$Journal,
                                        object$YearPublication
                                        ))))
          }
)

# NumberMatrices
#' The number of CompadreMat objects contained in a CompadreDB object (i.e. the)
#' number of projection matrices).
#' @rdname CompadreDataMethods
#' @export
setGeneric("NumberMatrices", 
               function(object, ...){
                   standardGeneric("NumberMatrices")
               }
)
#' @rdname CompadreDataMethods
#' @export
setMethod("NumberMatrices", signature = "CompadreDB", 
          function(object){
            return(dim(CompadreData(object))[1])
          }
)




