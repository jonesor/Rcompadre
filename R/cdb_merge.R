#' Merge two COM(P)ADRE databases together
#' 
#' Merges two CompadreDB objects via a row-bind of the data slots.
#' 
#' @param cdb1,cdb2 CompadreDB objects
#' 
#' @return A CompadreDB object containing both databases
#' 
#' @author Sam Levin
#' 
#' @examples 
#' Compadre1 <- subset(Compadre, Continent == "Asia")
#' Compadre2 <- subset(Compadre, Continent == "Africa")
#' 
#' cdb_merge(Compadre1, Compadre2)
#' 
#' @importFrom methods new
#' @export cdb_merge
cdb_merge <- function(cdb1, cdb2) {
  
  if (!inherits(cdb1, "CompadreDB") | !inherits(cdb2, "CompadreDB")) {
    stop("cdbs must be of class CompadreDB. See function as_cdb")
  }

  # cdbs must have matching columns to merge
  dat1 <- CompadreData(cdb1)
  dat2 <- CompadreData(cdb2)
  if(!identical(names(dat1), names(dat2))) {
    stop("Data components do not have identical names. ",
         "Make sure the data slot \n",
         "in each is identical to other.", Call. = FALSE)
  }
  
  # test whether cdbs have same version info
  vers1 <- VersionData(cdb1)
  vers2 <- VersionData(cdb2)
  
  if (isTRUE(all.equal(vers1, vers2))) {
    vers_out <- vers1
  } else {
    # if version info differs, set Version and DateCreated to NA
    vers_out <- vers1
    vers_out$Version <- NA_character_
    vers_out$DateCreated <- NA_character_
  }
  
  new('CompadreDB',
      data = rbind(dat1, dat2),
      version = vers_out)
}
