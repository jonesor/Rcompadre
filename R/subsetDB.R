#' Subsets the COMPADRE/COMADRE database
#' 
#' Subset the COMPADRE/COMADRE database by logical argument.
#' 
#' @param sub An argument made using logical operators (see `subset`) with
#' which to subset the data base. Any of the variables contained in the
#' metadata part of the COMPADRE/COMADRE database may be used.
#' @param db A COM(P)ADRE database object. Database versions <=4.0.1 will be coerced
#'  from class 'list'.
#' @return Returns a subset of the database, with the same structure, but where
#' the records in the metadata match the criteria given in the `sub` argument.
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @author Rob Salguero-Gómez <rob.salguero@@zoo.ox.ac.uk>
#' @author Bruce Kendall <kendall@@bren.ucsb.edu>
#' 
#' @examples
#' 
#' \dontrun{
#' ssData <- subsetDB(compadre, MatrixDimension > 3)
#' ssData <- subsetDB(compadre, MatrixDimension > 3 & MatrixComposite == "Mean")
#' ssData <- subsetDB(comadre, Continent == "Africa" & Class == "Mammalia")
#' ssData <- subsetDB(comadre, SurvivalIssue < 1 & Altitude > 1000 & Altitude < 1500)
#' }
#' 
#' @export subsetDB
subsetDB <- function(db, sub){
  # convert legacy versions of COM(P)ADRE from class 'list' to 'CompadreData'
  if (class(db) == "list"){
    if (as.numeric(gsub("\\.", "", sub("(\\s.*$)", "", db$version$Version))) <= 401){
      db <- as(db, "CompadreData")
    }
  }
  
  e <- substitute(sub)
  r <- eval(e, db@metadata, parent.frame())
  subsetID <- seq_len(length(r))[r & !is.na(r)]
 
  # First make a copy of the database.
  ssdb <- db

  # Subset the sub-parts of the database
  ssdb@metadata <- ssdb@metadata[subsetID,]
  ssdb@mat <- ssdb@mat[subsetID]
  
  # Version information is retained, but modified as follows.
  if("version" %in% slotNames(ssdb)){
    ssdb@version$Version <- paste(ssdb@version$Version,
                                  " - subset created on ",
                                  format(Sys.time(), 
                                         "%b_%d_%Y"),sep="")
    ssdb@version$DateCreated <- paste(ssdb@version$DateCreated,
                                      " - subset created on ",
                                      format(Sys.time(), 
                                             "%b_%d_%Y"),
                                      sep = "")
    ssdb@version$NumberAcceptedSpecies <- length(unique(ssdb@metadata$SpeciesAccepted))
    ssdb@version$NumberStudies <- length(unique(ssdb@metadata$SpeciesAuthor))
    ssdb@version$NumberMatrices <- length(ssdb@mat)
  }

  return(ssdb)
}
