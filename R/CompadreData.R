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
#' Needs description
#' 
#' @name CompadreData

setClass("CompadreData",
         slots = c(
             metadata = "data.frame",
             mat = "list",
             version = "list"
             )
         )



################################################################################
## Initialize & check

## define a method for initialize
#' @importFrom methods callNextMethod validObject
setMethod("initialize", "CompadreData",
    function(.Object, ...) {
        .Object <- methods::callNextMethod()
        methods::validObject(.Object)
        .Object
    })


## -----------------------------------------------------------------------------
## define validity check function
validCompadreData <- function(object) {
    errors <- character()
    if (nrow(object@metadata) != length(object@mat)) {
        msg <- paste0("Unequal metadata and mat lengths:",
                      nrow(object@metadata), ", ",
                      length(object@mat))
        errors <- c(errors, msg)
    }
    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }
}
setValidity("CompadreData", validCompadreData)



################################################################################
## define method to coerce old compadre db object to CompadreData class
setAs("list", "CompadreData", function(from) asCompadreData(from))

#' @importFrom methods new
asCompadreData <- function(from) {
    ## Need to check that 'from' is a old style compadre db object - this will
    ## have to be by checking it has the expected structure
    new("CompadreData",
        metadata = from$metadata,
        mat = lapply(seq_along(from$mat), function(i) {
          methods::new("CompadreM",
                       matA = from$mat[[i]]$matA,
                       matU = from$mat[[i]]$matU,
                       matF = from$mat[[i]]$matF,
                       matC = from$mat[[i]]$matC,
                       matrixClass = as.data.frame(from$matrixClass[[i]]))
        }),
        version = from$version)
}


################################################################################
## Methods

# show
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
#' their associated metadata. It's possible to pass numeric vectors, logical 
#' vectors, or character vectors that match the row or column names of the 
#' metadata. The function calls subsetDB to do the work of subsetting rows.
#' @rdname CompadreData
#' @export
setMethod(f = "[", signature = signature(x = "CompadreData", i = "ANY", j = "ANY", drop = "ANY"), 
    function(x, i, j, ..., drop = FALSE) {
    if(!missing(i)){
        if(!any(is.logical(i), is.numeric(i), is.character(i))) {
            stop("subset criteria must be logical, numeric (column / row numbers)\nor character (column / row names)")
        }
        if(is.logical(i)) i_logical <- i
        if(is.numeric(i)) {
            i_logical <- logical(NumberMatrices(x))
            i_logical[i] <- TRUE
        }
        if(is.character(i)){
            i_numeric <- match(i, row.names(x))
            i_logical <- logical(NumberMatrices(x))
            i_logical[i_numeric] <- TRUE
        }
    }
    if(!missing(j)){
        if(!any(is.logical(j), is.numeric(j), is.character(j))) {
            stop("subset criteria must be logical, numeric (column / row numbers)\nor character (column / row names)")
        }
    }
    xout <- subsetDB(x, i_logical)
    meta <- metadata(xout)[, j, drop = FALSE]
    xout@metadata <- meta
    xout
})


## -----------------------------------------------------------------------------
## metadata slot

# metadata
#' The metadata accessor function accesses the metadata of a CompadreData
#' object (it does not apply to CompadreM objects).
#' @rdname CompadreData
#' @export
setGeneric("metadata", 
               function(object, i, j){
                   standardGeneric("metadata")
               }
)
#' @rdname CompadreData
#' @export
setMethod("metadata", signature = "CompadreData", 
          function(object, i){
            return(object@metadata[ , ...])
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

#' @rdname CompadreData
#' @export
setGeneric("SpeciesAuthor", 
               function(object){
                   standardGeneric("SpeciesAuthor")
               }
)
#' @rdname CompadreData
#' @export
setMethod("SpeciesAuthor", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "SpeciesAuthor"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("SpeciesAccepted", 
               function(object){
                   standardGeneric("SpeciesAccepted")
               }
)
#' @rdname CompadreData
#' @export
setMethod("SpeciesAccepted", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "SpeciesAccepted"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("CommonName", 
               function(object){
                   standardGeneric("CommonName")
               }
)
#' @rdname CompadreData
#' @export
setMethod("CommonName", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "CommonName"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Infraspecific", 
               function(object){
                   standardGeneric("Infraspecific")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Infraspecific", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Infraspecific"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Genus", 
               function(object){
                   standardGeneric("Genus")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Genus", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Genus"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("GenusAccepted", 
               function(object){
                   standardGeneric("GenusAccepted")
               }
)
#' @rdname CompadreData
#' @export
setMethod("GenusAccepted", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "GenusAccepted"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("GenusAuthor", 
               function(object){
                   standardGeneric("GenusAuthor")
               }
)
#' @rdname CompadreData
#' @export
setMethod("GenusAuthor", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "GenusAuthor"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Family", 
               function(object){
                   standardGeneric("Family")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Family", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Family"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Order", 
               function(object){
                   standardGeneric("Order")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Order", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Order"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Class", 
               function(object){
                   standardGeneric("Class")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Class", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Class"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Phylum", 
               function(object){
                   standardGeneric("Phylum")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Phylum", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Phylum"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Kingdom", 
               function(object){
                   standardGeneric("Kingdom")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Kingdom", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Kingdom"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("OrganismType", 
               function(object){
                   standardGeneric("OrganismType")
               }
)
#' @rdname CompadreData
#' @export
setMethod("OrganismType", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "OrganismType"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("DicotMonoc", 
               function(object){
                   standardGeneric("DicotMonoc")
               }
)
#' @rdname CompadreData
#' @export
setMethod("DicotMonoc", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "DicotMonoc"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("AngioGymno", 
               function(object){
                   standardGeneric("AngioGymno")
               }
)
#' @rdname CompadreData
#' @export
setMethod("AngioGymno", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "AngioGymno"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Authors", 
               function(object){
                   standardGeneric("Authors")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Authors", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Authors"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Journal", 
               function(object){
                   standardGeneric("Journal")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Journal", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Journal"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("YearPublication", 
               function(object){
                   standardGeneric("YearPublication")
               }
)
#' @rdname CompadreData
#' @export
setMethod("YearPublication", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "YearPublication"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("DOI.ISBN", 
               function(object){
                   standardGeneric("DOI.ISBN")
               }
)
#' @rdname CompadreData
#' @export
setMethod("DOI.ISBN", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "DOI.ISBN"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("AdditionalSource", 
               function(object){
                   standardGeneric("AdditionalSource")
               }
)
#' @rdname CompadreData
#' @export
setMethod("AdditionalSource", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "AdditionalSource"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("StudyDuration", 
               function(object){
                   standardGeneric("StudyDuration")
               }
)
#' @rdname CompadreData
#' @export
setMethod("StudyDuration", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "StudyDuration"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("StudyStart", 
               function(object){
                   standardGeneric("StudyStart")
               }
)
#' @rdname CompadreData
#' @export
setMethod("StudyStart", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "StudyStart"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("StudyEnd", 
               function(object){
                   standardGeneric("StudyEnd")
               }
)
#' @rdname CompadreData
#' @export
setMethod("StudyEnd", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "StudyEnd"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("AnnualPeriodicity", 
               function(object){
                   standardGeneric("AnnualPeriodicity")
               }
)
#' @rdname CompadreData
#' @export
setMethod("AnnualPeriodicity", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "AnnualPeriodicity"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("NumberPopulations", 
               function(object){
                   standardGeneric("NumberPopulations")
               }
)
#' @rdname CompadreData
#' @export
setMethod("NumberPopulations", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "NumberPopulations"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixCriteriaSize", 
               function(object){
                   standardGeneric("MatrixCriteriaSize")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixCriteriaSize", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixCriteriaSize"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixCriteriaOntogeny", 
               function(object){
                   standardGeneric("MatrixCriteriaOntogeny")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixCriteriaOntogeny", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixCriteriaOntogeny"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixCriteriaAge", 
               function(object){
                   standardGeneric("MatrixCriteriaAge")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixCriteriaAge", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixCriteriaAge"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixPopulation", 
               function(object){
                   standardGeneric("MatrixPopulation")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixPopulation", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixPopulation"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Lat", 
               function(object){
                   standardGeneric("Lat")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Lat", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Lat"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Lon", 
               function(object){
                   standardGeneric("Lon")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Lon", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Lon"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Altitude", 
               function(object){
                   standardGeneric("Altitude")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Altitude", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Altitude"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Country", 
               function(object){
                   standardGeneric("Country")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Country", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Country"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Continent", 
               function(object){
                   standardGeneric("Continent")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Continent", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Continent"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Ecoregion", 
               function(object){
                   standardGeneric("Ecoregion")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Ecoregion", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Ecoregion"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("StudiedSex", 
               function(object){
                   standardGeneric("StudiedSex")
               }
)
#' @rdname CompadreData
#' @export
setMethod("StudiedSex", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "StudiedSex"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixComposite", 
               function(object){
                   standardGeneric("MatrixComposite")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixComposite", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixComposite"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixTreatment", 
               function(object){
                   standardGeneric("MatrixTreatment")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixTreatment", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixTreatment"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixCaptivity", 
               function(object){
                   standardGeneric("MatrixCaptivity")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixCaptivity", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixCaptivity"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixStartYear", 
               function(object){
                   standardGeneric("MatrixStartYear")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixStartYear", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixStartYear"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixStartSeason", 
               function(object){
                   standardGeneric("MatrixStartSeason")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixStartSeason", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixStartSeason"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixStartMonth", 
               function(object){
                   standardGeneric("MatrixStartMonth")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixStartMonth", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixStartMonth"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixEndYear", 
               function(object){
                   standardGeneric("MatrixEndYear")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixEndYear", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixEndYear"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixEndSeason", 
               function(object){
                   standardGeneric("MatrixEndSeason")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixEndSeason", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixEndSeason"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixEndMonth", 
               function(object){
                   standardGeneric("MatrixEndMonth")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixEndMonth", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixEndMonth"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixSplit", 
               function(object){
                   standardGeneric("MatrixSplit")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixSplit", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixSplit"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixFec", 
               function(object){
                   standardGeneric("MatrixFec")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixFec", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixFec"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("Observation", 
               function(object){
                   standardGeneric("Observation")
               }
)
#' @rdname CompadreData
#' @export
setMethod("Observation", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Observation"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("MatrixDimension", 
               function(object){
                   standardGeneric("MatrixDimension")
               }
)
#' @rdname CompadreData
#' @export
setMethod("MatrixDimension", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixDimension"])
          }
)

#' @rdname CompadreData
#' @export
setGeneric("SurvivalIssue", 
               function(object){
                   standardGeneric("SurvivalIssue")
               }
)
#' @rdname CompadreData
#' @export
setMethod("SurvivalIssue", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "SurvivalIssue"])
          }
)



## -----------------------------------------------------------------------------
## mat slot
# NOTE: several methods are contained and documents in ClassUnionMethods, 
# which apply to both CompadreData and CompadreM objects. These are
# currently:
# matA, matU, matF, matC

# mat
#' The mat accessor function accesses the matrices of a CompadreData object
#' object.
#' @rdname CompadreData
#' @export
setGeneric("mat",
               function(object, ...){
                   standardGeneric("mat")
               }
)
#' @rdname CompadreData
#' @export
setMethod("mat", signature = "CompadreData", 
          function(object){
            return(object@mat)
          }
)



## -----------------------------------------------------------------------------
## version slot

# Version
#' The version (including subset if relevant) of a CompadreData object.
#' @rdname CompadreData
#' @export
setGeneric("Version", 
               function(object, ...){
                   standardGeneric("Version")
               }
)
#' @rdname CompadreData
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
#' @rdname CompadreData
#' @export
setMethod("DateCreated", signature = "CompadreData", 
          function(object){
            return(object@version$DateCreated)
          }
)

# NumberAcceptedSpecies
#' The number of accepted binary species names in a CompadreData object.
#' @rdname CompadreData
#' @export
setGeneric("NumberAcceptedSpecies", 
               function(object, ...){
                   standardGeneric("NumberAcceptedSpecies")
               }
)
#' @rdname CompadreData
#' @export
setMethod("NumberAcceptedSpecies", signature = "CompadreData", 
          function(object){
            return(object@version$NumberAcceptedSpecies)
          }
)

# NumberStudies
#' The number of different studies in a CompadreData object.
#' @rdname CompadreData
#' @export
setGeneric("NumberStudies", 
               function(object, ...){
                   standardGeneric("NumberStudies")
               }
)
#' @rdname CompadreData
#' @export
setMethod("NumberStudies", signature = "CompadreData", 
          function(object){
            return(object@version$NumberStudies)
          }
)

# NumberMatrices
#' The number of CompadreM objects contained in a CompadreData object (i.e. the)
#' number of projection matrices).
#' @rdname CompadreData
#' @export
setGeneric("NumberMatrices", 
               function(object, ...){
                   standardGeneric("NumberMatrices")
               }
)
#' @rdname CompadreData
#' @export
setMethod("NumberMatrices", signature = "CompadreData", 
          function(object){
            return(object@version$NumberMatrices)
          }
)

