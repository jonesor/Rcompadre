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
  newdata1 <- data(db1)
  db2 <- convertLegacyDB(db2)
  newdata2 <- data(db2)

  # Probably don't want to combine databases without matching information
  if(!identical(names(metadata(db1)), names(metadata(db2)))) {
    stop("Metadata components do not have identical names. ",
         "Make sure the metadata \n",
         "in each is identical to other.", Call. = FALSE)
  }
  
  out <- methods::new('CompadreData',
                      data = rbind(data(db1),
                                   data(db2)),
                      version = VersionData(db1))
  
  # create indexes to check output
  seq1 <- seq_len(dim(metadata(db1))[1])
  seq2 <- seq(max(seq1) + 1, dim(metadata(out))[1], by = 1)
  
  if(!identical(mat(db1), 
                mat(out)[seq1]) |
     !identical(mat(db2), 
                mat(out)[seq2])) {
    
    # Not sure how to report this as an error other than it being my mistake
    stop("Something went wrong. Send a reproducible",
         " example to levisc8@gmail.com ")
  }
    
  # If the user hasn't used subset DB to create the smaller versions,
  # then add in that information. 
  # I will work on making these messages a bit prettier in the not
  # so distant future.
  outVersion <- VersionData(out)
  if(!grepl('subset created', Version(out))) {
    outVersion$Version <- paste(Version(out),
                                " - merge made on ",
                                format(Sys.time(), 
                                       "%b_%d_%Y"),
                                sep = "")
    
    outVersion$DateCreated <- paste(DateCreated(db1),
                                     " - merge made on ",
                                     format(Sys.time(), 
                                            "%b_%d_%Y"),
                                     sep = "") 
  }

  outVersion$NumberAcceptedSpecies <- NumberAcceptedSpecies(out)
  outVersion$NumberStudies <- NumberStudies(out)
  outVersion$NumberMatrices <- NumberMatrices(out)
  
  out@version <- outVersion
  return(out)
}
