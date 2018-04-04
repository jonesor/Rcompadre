#' Merge two Compadre/Comadre data sets together
#' 
#' @description Merges two data set objects together. These can either
#' be S4 objects of class \code{CompadreData}, S3 list objects that 
#' contain an older version of the database, or a combination of the
#' two. 
#' 
#' @param db1 A \code{CompadreData} object or an older version
#' of \code{COM(P)ADRE} in a \code{list}.
#' @param db2 A \code{CompadreData} object or an older version
#' of \code{COM(P)ADRE} in a \code{list}.
#' 
#' @return A \code{CompadreData} object containing both
#' databases.
#' 
#' @author Sam Levin
#' 
#' @examples 
#' \dontrun{
#' data(Compadre)
#' data(Comadre)
#' 
#' BigDB <- mergeDBs(Compadre, Comadre)
#' 
#' }
#' 
#' @importFrom methods new
#' 
#' @export mergeDBs
#' 
mergeDBs <- function(db1, db2) {
  
  db1 <- convertLegacyDB(db1)

  db2 <- convertLegacyDB(db2)
  # Probably don't want to combine databases without matching information
  if(!identical(names(db1@metadata), names(db2@metadata))) {
    stop("Metadata components do not have identical names. ",
         "Make sure the metadata \n",
         "in each is identical to other.", Call. = FALSE)
  }
  
  out <- methods::new('CompadreData',
                      metadata = rbind(db1@metadata,
                                       db2@metadata),
                      mat = c(db1@mat,
                              db2@mat),
                      version = db1@version)
  
  # create indexes to check output
  seq1 <- seq_len(dim(db1@metadata)[1])
  seq2 <- seq(max(seq1) + 1, dim(out@metadata)[1], by = 1)
  
  if(!identical(db1@mat, 
                out@mat[seq1]) |
     !identical(db2@mat, 
                out@mat[seq2])) {
    
    # Not sure how to report this as an error other than it being my mistake
    stop("Something went wrong. Send a reproducible",
         " example to levisc8@gmail.com ")
  }
    
  # If the user hasn't used subset DB to create the smaller versions,
  # then add in that information. 
  # I will work on making these messages a bit prettier in the not
  # so distant future.
  if(!grepl('subset created', out@version$Version)) {
    out@version$Version <- paste(db1@version$Version,
                                 " - merge made on ",
                                 format(Sys.time(), 
                                        "%b_%d_%Y"),
                                 sep = "")
    
    out@version$DateCreated <- paste(db1@version$DateCreated,
                                     " - merge made on ",
                                     format(Sys.time(), 
                                            "%b_%d_%Y"),
                                     sep = "") 
  }

  out@version$NumberAcceptedSpecies <- length(
    unique(out@metadata$SpeciesAccepted)
  )
  out@version$NumberStudies <- length(
    unique(
      paste0(out@metadata$Authors,
             out@metadata$Journal,
             out@metadata$YearPublication))
  )

  out@version$NumberMatrices <- length(out@mat)
  
  return(out)
}
