# Class CompadreData, definition and methods

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
#' CompadreData class
#' 
#' This page describes methods for accessing any metadata information from 
#' CompadreData objects.
#' 
#' @name CompadreMetadataMethods

setClass("CompadreData",
         slots = c(
             data = "data.frame",
             version = "list"
             )
         )



################################################################################
## Initialize & check

## define a method for initialize (does not need to be documented)
#' @importFrom methods callNextMethod validObject
setMethod("initialize", "CompadreData",
    function(.Object, ...) {
        .Object <- methods::callNextMethod()
        methods::validObject(.Object)
        .Object
    })


## -----------------------------------------------------------------------------
## define validity check function (does not need to be documented)
validCompadreData <- function(object) {
    dat <- object@data
    errors <- character()
    if (!("mat" %in% names(dat))) {
        msg <- 
"data must contain a column 'mat' containing matrices\n
 in the form of a list of 'ConmpadreM' objects"
        errors <- c(errors, msg)
    }
    if ("mat" %in% names(dat)) {
        if (!(class(dat$mat) %in% "list")) {
            msg <- "column 'mat' must be a list column"
            errors <- c(errors, msg)
        }
        if (class(dat$mat) %in% "list") {
            if (!(all(sapply(dat$mat, class) %in% "CompadreM"))) {
                msg <- "all elements of 'mat' must be 'CompadreM' objects"
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
setValidity("CompadreData", validCompadreData)



################################################################################
## Working with all data (matrices and metadata)

## -----------------------------------------------------------------------------
## define method to coerce old compadre db object to CompadreData class
setAs("list", "CompadreData", function(from) asCompadreData(from))

#' This page describes methods for working with the entire database (including 
#' both matrices and metadata) using CompadreData objects.
#' @name CompadreDataMethods
#' @export
#' @importFrom methods new
asCompadreData <- function(from) {
    # is all the data required to fill S4 slots there?
    if(!all(c( "metadata", "matrixClass", "mat", "version") %in% names(from))) {
        stop(
"This doesn't appear to be an old compadre data object. It does\n
 not contain all of the components 'metadata', 'mat', 'matrixClass'\n
 and 'version' required to generate a CompadreData object. See ?CompadreData")
    }
    # do all the matrix list elements contain matA, matF, matU and matC?
    matNames <- sapply(from$mat, names)
    matNamesMatch <- all(apply(matNames, 
                         2, 
                         function(N){ N == c("matA", "matU", "matF", "matC") }))
    if(!matNamesMatch) {
        stop(
"Not all matrices in this compadre data object contain all 
 of the components matA, matU, matF and matC (in that order) 
 required to generate CompadreM objects. See ?CompadreM"
        )
    }
    # get metadata
    metadata <- from$metadata
    # get matrices and coerce into CompadreM objects
    mat <- lapply(seq_along(from$mat), function(i) {
        methods::new("CompadreM",
                    matA = from$mat[[i]]$matA,
                    matU = from$mat[[i]]$matU,
                    matF = from$mat[[i]]$matF,
                    matC = from$mat[[i]]$matC,
                    matrixClass = as.data.frame(from$matrixClass[[i]]))
    })
    # add matrices to metadata as a list column
    dat <- metadata
    dat$mat <- mat
    # create a new CompadreData object with the new data and version
    new("CompadreData",
        data = dat,
        version = from$version)
}


## -----------------------------------------------------------------------------
## define a method for showing the object (does not need to be documented)
setMethod("show", signature = (object ="CompadreData"),
          function (object){
            Mno <- NumberMatrices(object)
            Sno <- NumberAcceptedSpecies(object)
            if(is.character(Version(object))) { V <- paste("Version:",
                                                           Version(object))
                                              }
            if(!is.character(Version(object))) V <- "Unknown"
            #start
            cat(paste("A com(p)adre database ('CompadreData') object with ",
                      as.character(Sno),
                      " SPECIES and ",
                      as.character(Mno),
                      " MATRICES.\n",
                      "See ?CompadreData and ?CompadreUnionMethods for methods of accessing data.\n\n",
                      sep = ""
            ))
          }
)


## -----------------------------------------------------------------------------
# Subset

#' This method enables subsetting the data using square brackets, i.e. for a
#' CompadreData object "DB" one can use DB[1:2, 1:10] to get a new CompadreData
#' object that includes the first two matrices and the the first 10 columns of 
#' their associated metadata. It's possible to pass logical vectors, numeric
#' vectors, or character vectors that match the row or column names of the 
#' metadata. 
#' @rdname CompadreDataMethods
#' @export
setMethod(f = "[", signature = signature(x = "CompadreData", i = "ANY", j = "ANY", drop = "ANY"), 
    function(x, i, j, ..., drop = FALSE) {
    if(!missing(i)){
        if(!any(is.logical(i), is.numeric(i), is.character(i))) {
            stop("subset criteria must be logical, numeric (column / row numbers)\nor character (column / row names)")
        }
    }
    if(!missing(j)){
        if(!any(is.logical(j), is.numeric(j), is.character(j))) {
            stop("subset criteria must be logical, numeric (column / row numbers)\nor character (column / row names)")
        }
        cols <- j
        if(is.logical(j) & !j[1]){
            warning("'mat' was included in the output by default, although not selected")
            cols[1] <- TRUE
        }
        if(is.numeric(j) & !(1 %in% j)){
            warning("'mat' was included in the output by default, although not selected")
            cols <- c(1, j)
        }
        if(is.character(j) & !(names(data(x))[1] %in% j)){
            warning("'mat' was included in the output by default, although not selected")
            cols <- c(names(data(x))[1], j)
        }
    }
    dat <- data(x)[i, j, drop = FALSE]
    ver <- Version(x)
    ver$Version <- paste0(x@version$Version,
                          " - subset created on ",
                          format(Sys.time(), "%b_%d_%Y")
                         )
    ver$DateCreated <- paste0(x@version$DateCreated,
                              " - subset created on ",
                              format(Sys.time(), "%b_%d_%Y")
                             )
    ver$NumberAcceptedSpecies <- length(unique(x@metadata$SpeciesAccepted))
    ver$NumberStudies <- length(unique(paste0(x@metadata$Authors,
                                              x@metadata$Journal,
                                              x@metadata$YearPublication
                                             )))
    ssdb@version$NumberMatrices <- dim(x@data)[1]
    xout <- methods::new(CompadreData, data = dat, version = ver)
    xout
})


## -----------------------------------------------------------------------------
## data slot

# data
#' The data accessor function accesses the data of a CompadreData
#' object, which includes both the matrices and the metadata.
#' The matrices are included as a list column of CompadreM objects, 
#' with the variable (column) name 'mat'.
#' @rdname CompadreDataMethods
#' @export
setGeneric("data", 
               function(object){
                   standardGeneric("data")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("data", signature = "CompadreData", 
          function(object){
            return(object@data)
          }
)

################################################################################
## Working with just the metadata

# metadata
#' The metadata accessor function accesses the metadata of a CompadreData
#' object, which is a data frame including all variables, but excluding the 
#' matrices. Further methods described below allow the user to access 
#' individual metadata variables as vectors.
#' @rdname CompadreMetadataMethods
#' @export
setGeneric("metadata", 
               function(object){
                   standardGeneric("metadata")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("metadata", signature = "CompadreData", 
          function(object){
            return(object@data[ , !names(object@data %in% "mat")])
          }
)

# Metadata variables
# These functions access certain variables from the metadata slot of a
# CompadreData object. These methods are implemented using 
# VariableName(CompadreDataObject) and are effectively the same as using 
# `metadata(CompadreDataObject, "VariableName")`. Most variables are available 
# for both compadre and comadre, but some are only available for either one or 
# the other. Where a variable is not available for the chosen database, the 
# function will simply return an error. The variables are: 
# "SpeciesAuthor", "SpeciesAccepted", "CommonName", "Infraspecific" [comadre], 
# "Genus" [compadre] or "GenusAccepted" [comadre], "GenusAuthor" [comadre],
# "Family", "Order" , "Class", "Phylum", "Kingdom", 
# "OrganismType", "DicotMonoc", [compadre], "AngioGymno" [compadre],
# "Authors", "Journal", "YearPublication", "DOI.ISBN", "AdditionalSource",
# "StudyDuration", "StudyStart", "StudyEnd", "AnnualPeriodicity",
# "NumberPopulations", "MatrixCriteriaSize", "MatrixCriteriaOntogeny",
# "MatrixCriteriaAge", "MatrixPopulation", 
# "Lat", "Lon", "Altitude", "Country", "Continent", "Ecoregion",
# "StudiedSex",
# "MatrixComposite", "MatrixTreatment", "MatrixCaptivity",
# "MatrixStartYear", "MatrixStartSeason", "MatrixStartMonth",
# "MatrixEndYear", "MatrixEndSeason", "MatrixEndMonth",
# "MatrixSplit", "MatrixFec", "Observation",
# "MatrixDimension", "SurvivalIssue".

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("SpeciesAuthor", 
               function(object){
                   standardGeneric("SpeciesAuthor")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("SpeciesAuthor", signature = "CompadreData", 
          function(object){
            return(object@data[ , "SpeciesAuthor"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("SpeciesAccepted", 
               function(object){
                   standardGeneric("SpeciesAccepted")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("SpeciesAccepted", signature = "CompadreData", 
          function(object){
            return(object@data[ , "SpeciesAccepted"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("CommonName", 
               function(object){
                   standardGeneric("CommonName")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("CommonName", signature = "CompadreData", 
          function(object){
            return(object@data[ , "CommonName"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Infraspecific", 
               function(object){
                   standardGeneric("Infraspecific")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Infraspecific", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Infraspecific"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Genus", 
               function(object){
                   standardGeneric("Genus")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Genus", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Genus"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("GenusAccepted", 
               function(object){
                   standardGeneric("GenusAccepted")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("GenusAccepted", signature = "CompadreData", 
          function(object){
            return(object@data[ , "GenusAccepted"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("GenusAuthor", 
               function(object){
                   standardGeneric("GenusAuthor")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("GenusAuthor", signature = "CompadreData", 
          function(object){
            return(object@data[ , "GenusAuthor"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Family", 
               function(object){
                   standardGeneric("Family")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Family", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Family"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Order", 
               function(object){
                   standardGeneric("Order")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Order", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Order"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Class", 
               function(object){
                   standardGeneric("Class")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Class", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Class"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Phylum", 
               function(object){
                   standardGeneric("Phylum")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Phylum", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Phylum"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Kingdom", 
               function(object){
                   standardGeneric("Kingdom")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Kingdom", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Kingdom"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("OrganismType", 
               function(object){
                   standardGeneric("OrganismType")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("OrganismType", signature = "CompadreData", 
          function(object){
            return(object@data[ , "OrganismType"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("DicotMonoc", 
               function(object){
                   standardGeneric("DicotMonoc")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("DicotMonoc", signature = "CompadreData", 
          function(object){
            return(object@data[ , "DicotMonoc"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("AngioGymno", 
               function(object){
                   standardGeneric("AngioGymno")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("AngioGymno", signature = "CompadreData", 
          function(object){
            return(object@data[ , "AngioGymno"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Authors", 
               function(object){
                   standardGeneric("Authors")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Authors", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Authors"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Journal", 
               function(object){
                   standardGeneric("Journal")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Journal", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Journal"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("YearPublication", 
               function(object){
                   standardGeneric("YearPublication")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("YearPublication", signature = "CompadreData", 
          function(object){
            return(object@data[ , "YearPublication"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("DOI.ISBN", 
               function(object){
                   standardGeneric("DOI.ISBN")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("DOI.ISBN", signature = "CompadreData", 
          function(object){
            return(object@data[ , "DOI.ISBN"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("AdditionalSource", 
               function(object){
                   standardGeneric("AdditionalSource")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("AdditionalSource", signature = "CompadreData", 
          function(object){
            return(object@data[ , "AdditionalSource"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("StudyDuration", 
               function(object){
                   standardGeneric("StudyDuration")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("StudyDuration", signature = "CompadreData", 
          function(object){
            return(object@data[ , "StudyDuration"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("StudyStart", 
               function(object){
                   standardGeneric("StudyStart")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("StudyStart", signature = "CompadreData", 
          function(object){
            return(object@data[ , "StudyStart"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("StudyEnd", 
               function(object){
                   standardGeneric("StudyEnd")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("StudyEnd", signature = "CompadreData", 
          function(object){
            return(object@data[ , "StudyEnd"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("AnnualPeriodicity", 
               function(object){
                   standardGeneric("AnnualPeriodicity")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("AnnualPeriodicity", signature = "CompadreData", 
          function(object){
            return(object@data[ , "AnnualPeriodicity"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("NumberPopulations", 
               function(object){
                   standardGeneric("NumberPopulations")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("NumberPopulations", signature = "CompadreData", 
          function(object){
            return(object@data[ , "NumberPopulations"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixCriteriaSize", 
               function(object){
                   standardGeneric("MatrixCriteriaSize")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixCriteriaSize", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixCriteriaSize"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixCriteriaOntogeny", 
               function(object){
                   standardGeneric("MatrixCriteriaOntogeny")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixCriteriaOntogeny", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixCriteriaOntogeny"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixCriteriaAge", 
               function(object){
                   standardGeneric("MatrixCriteriaAge")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixCriteriaAge", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixCriteriaAge"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixPopulation", 
               function(object){
                   standardGeneric("MatrixPopulation")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixPopulation", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixPopulation"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Lat", 
               function(object){
                   standardGeneric("Lat")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Lat", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Lat"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Lon", 
               function(object){
                   standardGeneric("Lon")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Lon", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Lon"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Altitude", 
               function(object){
                   standardGeneric("Altitude")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Altitude", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Altitude"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Country", 
               function(object){
                   standardGeneric("Country")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Country", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Country"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Continent", 
               function(object){
                   standardGeneric("Continent")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Continent", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Continent"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Ecoregion", 
               function(object){
                   standardGeneric("Ecoregion")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Ecoregion", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Ecoregion"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("StudiedSex", 
               function(object){
                   standardGeneric("StudiedSex")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("StudiedSex", signature = "CompadreData", 
          function(object){
            return(object@data[ , "StudiedSex"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixComposite", 
               function(object){
                   standardGeneric("MatrixComposite")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixComposite", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixComposite"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixTreatment", 
               function(object){
                   standardGeneric("MatrixTreatment")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixTreatment", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixTreatment"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixCaptivity", 
               function(object){
                   standardGeneric("MatrixCaptivity")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixCaptivity", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixCaptivity"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixStartYear", 
               function(object){
                   standardGeneric("MatrixStartYear")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixStartYear", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixStartYear"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixStartSeason", 
               function(object){
                   standardGeneric("MatrixStartSeason")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixStartSeason", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixStartSeason"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixStartMonth", 
               function(object){
                   standardGeneric("MatrixStartMonth")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixStartMonth", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixStartMonth"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixEndYear", 
               function(object){
                   standardGeneric("MatrixEndYear")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixEndYear", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixEndYear"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixEndSeason", 
               function(object){
                   standardGeneric("MatrixEndSeason")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixEndSeason", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixEndSeason"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixEndMonth", 
               function(object){
                   standardGeneric("MatrixEndMonth")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixEndMonth", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixEndMonth"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixSplit", 
               function(object){
                   standardGeneric("MatrixSplit")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixSplit", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixSplit"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixFec", 
               function(object){
                   standardGeneric("MatrixFec")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixFec", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixFec"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Observation", 
               function(object){
                   standardGeneric("Observation")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Observation", signature = "CompadreData", 
          function(object){
            return(object@data[ , "Observation"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("MatrixDimension", 
               function(object){
                   standardGeneric("MatrixDimension")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("MatrixDimension", signature = "CompadreData", 
          function(object){
            return(object@data[ , "MatrixDimension"])
          }
)

#' @rdname CompadreMetadataMethods
#' @export
setGeneric("SurvivalIssue", 
               function(object){
                   standardGeneric("SurvivalIssue")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("SurvivalIssue", signature = "CompadreData", 
          function(object){
            return(object@data[ , "SurvivalIssue"])
          }
)


################################################################################
## Working with just matrices
## (Note most relevant functions for working with matrices are contained in
## 'ClassUnionMethods.R' and described in the 'CompdareMatrixMethods' rd file)

## -----------------------------------------------------------------------------
## mat column

# mat
#' The mat accessor function accesses the matrices of a CompadreData object
#' object.
#' @rdname CompadreMatrixMethods
#' @export
setGeneric("mat",
               function(object, ...){
                   standardGeneric("mat")
               }
)
#' @rdname CompadreMatrixMethods
#' @export
setMethod("mat", signature = "CompadreData", 
          function(object){
            return(object@mat)
          }
)


## -----------------------------------------------------------------------------
## version slot

################################################################################
## Working with version information

# All version data
#' All version information (including subset information) for a CompadreData 
#' object, as a list.
#' @rdname CompadreMetadataMethods
#' @export
setGeneric("VersionData", 
               function(object, ...){
                   standardGeneric("VersionData")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("VersionData", signature = "CompadreData", 
          function(object){
            return(object@version)
          }
)

# Version
#' The version (including subset if relevant) of a CompadreData object.
#' @rdname CompadreMetadataMethods
#' @export
setGeneric("Version", 
               function(object, ...){
                   standardGeneric("Version")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("Version", signature = "CompadreData", 
          function(object){
            return(object@version$Version)
          }
)

# DateCreated
#' The date a CompadreData Version was created.
#' @rdname DateCreated
#' @export
setGeneric("DateCreated", 
               function(object, ...){
                   standardGeneric("DateCreated")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("DateCreated", signature = "CompadreData", 
          function(object){
            return(object@version$DateCreated)
          }
)

# NumberAcceptedSpecies
#' The number of accepted binary species names in a CompadreData object.
#' @rdname CompadreMetadataMethods
#' @export
setGeneric("NumberAcceptedSpecies", 
               function(object, ...){
                   standardGeneric("NumberAcceptedSpecies")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("NumberAcceptedSpecies", signature = "CompadreData", 
          function(object){
            return(length(unique(SpeciesAccepted(object))))
          }
)

# NumberStudies
#' The number of different studies in a CompadreData object.
#' @rdname CompadreMetadataMethods
#' @export
setGeneric("NumberStudies", 
               function(object, ...){
                   standardGeneric("NumberStudies")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("NumberStudies", signature = "CompadreData", 
          function(object){
            return(length(unique(paste0(Authors(object),
                                        Journal(object),
                                        YearPublication(object)
                                        ))))
          }
)

# NumberMatrices
#' The number of CompadreM objects contained in a CompadreData object (i.e. the)
#' number of projection matrices).
#' @rdname CompadreMetadataMethods
#' @export
setGeneric("NumberMatrices", 
               function(object, ...){
                   standardGeneric("NumberMatrices")
               }
)
#' @rdname CompadreMetadataMethods
#' @export
setMethod("NumberMatrices", signature = "CompadreData", 
          function(object){
            return(dim(data(object)[1]))
          }
)

