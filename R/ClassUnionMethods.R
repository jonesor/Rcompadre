
## Set class union between CompadreM and CompadreData, so the same accessor 
## functions can be used for both

################################################################################
#' Accessor methods for CompadreM and CompadreData objects
#'
#' This page describes methods for both the CompadreM and CompadreData classes. 
#' In many cases the same method can be used for both classes, but some methods
#' are only valid for one class.
#' 
#' @rdname ClassUnionMethods

# Set class union for the two classes. Each class 'contains' the class union, 
# so methods set for the union can be used by both classes.
setClassUnion("CompadreMorData", c("CompadreM", "CompadreData"))

# matA
#' @rdname ClassUnionMethods
#' @export
setGeneric("matA", 
               function(object){
                   standardGeneric("matA")
               }
)
#' @rdname ClassUnionMethods
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
#' @rdname ClassUnionMethods
#' @export
setGeneric("matU", 
               function(object){
                   standardGeneric("matU")
               }
)
#' @rdname ClassUnionMethods
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
#' @rdname ClassUnionMethods
#' @export
setGeneric("matF", 
               function(object){
                   standardGeneric("matF")
               }
)
#' @rdname ClassUnionMethods
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
#' @rdname ClassUnionMethods
#' @export
setGeneric("matC", 
               function(object){
                   standardGeneric("matC")
               }
)
#' @rdname ClassUnionMethods
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

# matrixClass
#' @rdname ClassUnionMethods
#' @export
setGeneric("matrixClass", 
               function(object){
                   standardGeneric("matrixClass")
               }
)
#' @rdname ClassUnionMethods
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

# metadata
#' The metadata accessor function accesses the metadata of a CompadreData
#' object (it does not apply to CompadreM objects).
#' @rdname ClassUnionMethods
#' @export
setGeneric("metadata", 
               function(object, ...){
                   standardGeneric("metadata")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("metadata", signature = "CompadreData", 
          function(object, ...){
            return(object@metadata[ , ...])
          }
)

#' Metadata variables
#' These functions access certain variables from the metadata slot of a
#' CompadreData object. These methods are implemented using 
#' VariableName(CompadreDataObject) and are effectively the same as using 
#' `metadata(CompadreDataObject, "VariableName")`. Most variables are available 
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
#' @rdname ClassUnionMethods
#' @export
setGeneric("SpeciesAuthor", 
               function(object){
                   standardGeneric("SpeciesAuthor")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("SpeciesAuthor", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "SpeciesAuthor"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("SpeciesAccepted", 
               function(object){
                   standardGeneric("SpeciesAccepted")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("SpeciesAccepted", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "SpeciesAccepted"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("CommonName", 
               function(object){
                   standardGeneric("CommonName")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("CommonName", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "CommonName"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Infraspecific", 
               function(object){
                   standardGeneric("Infraspecific")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Infraspecific", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Infraspecific"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Genus", 
               function(object){
                   standardGeneric("Genus")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Genus", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Genus"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("GenusAccepted", 
               function(object){
                   standardGeneric("GenusAccepted")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("GenusAccepted", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "GenusAccepted"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("GenusAuthor", 
               function(object){
                   standardGeneric("GenusAuthor")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("GenusAuthor", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "GenusAuthor"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Family", 
               function(object){
                   standardGeneric("Family")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Family", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Family"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Order", 
               function(object){
                   standardGeneric("Order")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Order", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Order"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Class", 
               function(object){
                   standardGeneric("Class")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Class", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Class"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Phylum", 
               function(object){
                   standardGeneric("Phylum")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Phylum", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Phylum"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Kingdom", 
               function(object){
                   standardGeneric("Kingdom")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Kingdom", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Kingdom"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("OrganismType", 
               function(object){
                   standardGeneric("OrganismType")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("OrganismType", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "OrganismType"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("DicotMonoc", 
               function(object){
                   standardGeneric("DicotMonoc")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("DicotMonoc", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "DicotMonoc"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("AngioGymno", 
               function(object){
                   standardGeneric("AngioGymno")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("AngioGymno", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "AngioGymno"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Authors", 
               function(object){
                   standardGeneric("Authors")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Authors", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Authors"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Journal", 
               function(object){
                   standardGeneric("Journal")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Journal", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Journal"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("YearPublication", 
               function(object){
                   standardGeneric("YearPublication")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("YearPublication", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "YearPublication"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("DOI.ISBN", 
               function(object){
                   standardGeneric("DOI.ISBN")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("DOI.ISBN", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "DOI.ISBN"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("AdditionalSource", 
               function(object){
                   standardGeneric("AdditionalSource")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("AdditionalSource", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "AdditionalSource"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("StudyDuration", 
               function(object){
                   standardGeneric("StudyDuration")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("StudyDuration", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "StudyDuration"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("StudyStart", 
               function(object){
                   standardGeneric("StudyStart")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("StudyStart", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "StudyStart"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("StudyEnd", 
               function(object){
                   standardGeneric("StudyEnd")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("StudyEnd", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "StudyEnd"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("AnnualPeriodicity", 
               function(object){
                   standardGeneric("AnnualPeriodicity")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("AnnualPeriodicity", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "AnnualPeriodicity"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("NumberPopulations", 
               function(object){
                   standardGeneric("NumberPopulations")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("NumberPopulations", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "NumberPopulations"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixCriteriaSize", 
               function(object){
                   standardGeneric("MatrixCriteriaSize")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixCriteriaSize", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixCriteriaSize"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixCriteriaOntogeny", 
               function(object){
                   standardGeneric("MatrixCriteriaOntogeny")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixCriteriaOntogeny", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixCriteriaOntogeny"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixCriteriaAge", 
               function(object){
                   standardGeneric("MatrixCriteriaAge")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixCriteriaAge", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixCriteriaAge"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixPopulation", 
               function(object){
                   standardGeneric("MatrixPopulation")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixPopulation", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixPopulation"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Lat", 
               function(object){
                   standardGeneric("Lat")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Lat", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Lat"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Lon", 
               function(object){
                   standardGeneric("Lon")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Lon", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Lon"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Altitude", 
               function(object){
                   standardGeneric("Altitude")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Altitude", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Altitude"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Country", 
               function(object){
                   standardGeneric("Country")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Country", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Country"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Continent", 
               function(object){
                   standardGeneric("Continent")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Continent", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Continent"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Ecoregion", 
               function(object){
                   standardGeneric("Ecoregion")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Ecoregion", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Ecoregion"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("StudiedSex", 
               function(object){
                   standardGeneric("StudiedSex")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("StudiedSex", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "StudiedSex"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixComposite", 
               function(object){
                   standardGeneric("MatrixComposite")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixComposite", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixComposite"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixTreatment", 
               function(object){
                   standardGeneric("MatrixTreatment")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixTreatment", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixTreatment"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixCaptivity", 
               function(object){
                   standardGeneric("MatrixCaptivity")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixCaptivity", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixCaptivity"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixStartYear", 
               function(object){
                   standardGeneric("MatrixStartYear")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixStartYear", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixStartYear"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixStartSeason", 
               function(object){
                   standardGeneric("MatrixStartSeason")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixStartSeason", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixStartSeason"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixStartMonth", 
               function(object){
                   standardGeneric("MatrixStartMonth")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixStartMonth", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixStartMonth"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixEndYear", 
               function(object){
                   standardGeneric("MatrixEndYear")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixEndYear", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixEndYear"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixEndSeason", 
               function(object){
                   standardGeneric("MatrixEndSeason")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixEndSeason", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixEndSeason"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixEndMonth", 
               function(object){
                   standardGeneric("MatrixEndMonth")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixEndMonth", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixEndMonth"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixSplit", 
               function(object){
                   standardGeneric("MatrixSplit")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixSplit", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixSplit"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixFec", 
               function(object){
                   standardGeneric("MatrixFec")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixFec", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixFec"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("Observation", 
               function(object){
                   standardGeneric("Observation")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("Observation", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "Observation"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("MatrixDimension", 
               function(object){
                   standardGeneric("MatrixDimension")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("MatrixDimension", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "MatrixDimension"])
          }
)

#' @rdname ClassUnionMethods
#' @export
setGeneric("SurvivalIssue", 
               function(object){
                   standardGeneric("SurvivalIssue")
               }
)
#' @rdname ClassUnionMethods
#' @export
setMethod("SurvivalIssue", signature = "CompadreData", 
          function(object){
            return(object@metadata[ , "SurvivalIssue"])
          }
)



