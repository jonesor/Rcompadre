#' Subsets the COMPADRE/COMADRE database
#' 
#' Subset the COMPADRE/COMADRE database by logical argument.
#' 
#' @param db A COM(P)ADRE database object. Databases will be will be coerced
#'  from the old 'list' format where appropriate (compadre_v4.0.1 and below; 
#' comadre_v2.0.1 and below).
#' @param sub An argument made using logical operators (see `subset`) with
#' which to subset the data base. Any of the variables contained in the
#' metadata part of the COMPADRE/COMADRE database may be used.
#' 
#' @return Returns a subset of the database, with the same structure, but where
#' the records in the metadata match the criteria given in the `sub` argument.
#' 
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @author Rob Salguero-Gómez <rob.salguero@@zoo.ox.ac.uk>
#' @author Bruce Kendall <kendall@@bren.ucsb.edu>
#' 
#' @examples
#' \dontrun{
#' ssData <- subsetDB(compadre, MatrixDimension > 3)
#' ssData <- subsetDB(compadre, MatrixDimension > 3 & MatrixComposite == "Mean")
#' ssData <- subsetDB(comadre, Continent == "Africa" & Class == "Mammalia")
#' ssData <- subsetDB(comadre, Altitude > 1000 & Altitude < 1500)
#' }
#' 
#' @importFrom methods as slotNames
#' @export subsetDB
#' 
subsetDB <- function(db, sub){
  # convert legacy versions of COM(P)ADRE from class 'list' to 'CompadreData'
  if (class(db) == "list"){
    if( "Animalia" %in% db$metadata$Kingdom ) vlim <- 201
    if( "Plantae" %in% db$metadata$Kingdom ) vlim <- 401
    if (as.numeric(gsub("\\.", "", sub("(\\s.*$)", "", db$version$Version))) <= vlim){
      db <- methods::as(db, "CompadreData")
    }
  }
  
  e <- substitute(sub)
  r <- eval(e, db@metadata, parent.frame())
  subsetID <- seq_len(length(r))[r & !is.na(r)]
  
  # First make a copy of the database
  ssdb <- db

  # Subset the sub-parts of the database
  ssdb@metadata <- ssdb@metadata[subsetID,]
  ssdb@mat <- ssdb@mat[subsetID]
  
  # Version information is retained, but modified as follows.
  if("version" %in% methods::slotNames(ssdb)){
    ssdb@version$Version <- paste0(ssdb@version$Version,
                                   " - subset created on ",
                                   format(Sys.time(), "%b_%d_%Y")
                                  )
    ssdb@version$DateCreated <- paste0(ssdb@version$DateCreated,
                                       " - subset created on ",
                                       format(Sys.time(), "%b_%d_%Y")
                                      )
    ssdb@version$NumberAcceptedSpecies <- length(unique(ssdb@metadata$SpeciesAccepted))
    ssdb@version$NumberStudies <- length(unique(paste0(ssdb@metadata$Authors,
                                                       ssdb@metadata$Journal,
                                                       ssdb@metadata$YearPublication
                                                      )))
    ssdb@version$NumberMatrices <- length(ssdb@mat)
  }
  return(ssdb)
}