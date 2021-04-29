#' Merge two COM(P)ADRE databases via row-bind
#' 
#' Merges two CompadreDB objects via a row-bind of the data slots.
#' 
#' @param cdb1,cdb2 CompadreDB objects
#' 
#' @return A CompadreDB object created by binding the rows of \code{cdb1} and
#'   \code{cdb2}
#' 
#' @author Sam Levin <levisc8@@gmail.com>
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' 
#' @family data management
#' 
#' @examples 
#' Compadre1 <- subset(Compadre, Continent == "Asia")
#' Compadre2 <- subset(Compadre, Continent == "Africa")
#' 
#' cdb_rbind(Compadre1, Compadre2)
#' 
#' @importFrom methods new
#' @export cdb_rbind
cdb_rbind <- function(cdb1, cdb2) {
  
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
