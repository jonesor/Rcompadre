#' Merge two Compadre/Comadre databases together
#' 
#' @description Merges two CompadreDB objects together.
#' 
#' @param db1 A \code{CompadreDB} object
#' @param db2 A \code{CompadreDB} object
#' 
#' @return A \code{CompadreDB} object containing both databases.
#' 
#' @author Sam Levin
#' 
#' @examples 
#' \dontrun{
#' CompadreData(Compadre)
#' CompadreData(Comadre)
#' 
#' BigDB <- mergeDBs(Compadre, Comadre)
#' }
#' 
#' @importFrom methods new
#' @export mergeDBs
mergeDBs <- function(db1, db2) {
  
  if (!inherits(db1, "CompadreDB") | !inherits(db2, "CompadreDB")) {
    stop("dbs must be of class CompadreDB. See function convertLegacyDB")
  }

  # dbs must have matching columns to merge
  dat1 <- CompadreData(db1)
  dat2 <- CompadreData(db2)
  if(!identical(names(dat1), names(dat2))) {
    stop("Metadata components do not have identical names. ",
         "Make sure the metadata \n",
         "in each is identical to other.", Call. = FALSE)
  }
  
  # test whether dbs have same version info
  vers1 <- VersionData(db1)
  vers2 <- VersionData(db2)
  
  if (all.equal(vers1, vers2)) {
    vers_out <- vers1
  } else {
    # if version info differs, set Version and DateCreated to NA
    vers_out <- vers1
    vers_out$Version <- NA_character_
    vers_out$DateCreated <- NA_character_
  }
  
  new('CompadreDB',
      CompadreData = rbind(dat1, dat2),
      VersionData = vers_out)
}
