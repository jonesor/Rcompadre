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
  db <- methods::as(db, "CompadreDB")
  
  e <- substitute(sub)
  mdat <- CompadreData(db)[!(names(CompadreData(db)) %in% "mat")]
  r <- eval(e, mdat, parent.frame())
  subsetID <- seq_len(length(r))[r & !is.na(r)]
  
  # Subset the sub-parts of the database
  ssdata <- CompadreData(db)[subsetID, ]
  
  # Version information is retained, but modified as follows.
  ssversion <- VersionData(db)
  ssversion$Version <- paste0(Version(db),
                                  " - subset created on ",
                                  format(Sys.time(), "%b_%d_%Y")
                                )
  ssversion$DateCreated <- paste0(DateCreated(db),
                                      " - subset created on ",
                                      format(Sys.time(), "%b_%d_%Y")
                                    )
  ssversion$NumberAcceptedSpecies <- length(unique(ssdata$SpeciesAccepted))
  ssversion$NumberStudies <- length(unique(paste0(ssdata$Authors,
                                                  ssdata$Journal,
                                                  ssdata$YearPublication
                                                    )))
  ssversion$NumberMatrices <- dim(ssdata)[1]

  # new database to return
  ssdb <- new("CompadreDB", CompadreData = ssdata, VersionData = ssversion)
  return(ssdb)
}

test <- subsetDB(compadre, SpeciesAccepted == "Alaria nana")
