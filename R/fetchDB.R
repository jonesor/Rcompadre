#' Fetch the COM(P)ADRE database from compadre-db.org or a local file
#'
#' Fetch the current version of the COM(P)ADRE database from
#' \url{https://compadre-db.org}, or load any version stored in a local .RData
#' file.
#'
#' @param cdb Either "comadre" or "compadre" (case insensitive) to fetch the
#'   most recent database from \url{https://compadre-db.org}, or a path to an
#'   existing COMPADRE database (i.e. .RData file) stored on the local machine.
#' 
#' @return A CompadreDB object
#' @author Patrick M. Barks <patrick.barks@@gmail.com>
#' 
#' @examples
#' \dontrun{
#' compadre <- cdb_fetch('compadre')
#' }
#' @export
cdb_fetch <- function(cdb) {
  
  # get url or path
  if (tolower(cdb) == 'comadre') {
    path <- url('https://www.compadre-db.org/Data/ComadreDownload')
  } else if (tolower(cdb) == 'compadre') {
    path <- url('https://www.compadre-db.org/Data/CompadreDownload')
  } else {
    path <- path.expand(cdb)
  }
  
  # fetch and load
  env <- new.env()
  x <- load(path, env)[1]
  dbFetch <- env[[x]]
  
  # convert to CompadreDB
  if (inherits(dbFetch, "CompadreDB")) {
    dbOut <- dbFetch
  } else {
    dbOut <- as_cdb(dbFetch)
  }
  
  # print db name, version, and release date
  name <- toupper(x)
  version <- Version(dbOut)
  release <- DateCreated(dbOut)
  
  message("This is ", name, " version ", version, " (release date ", release,
          ")", "\n", "See user agreement at ",
          "https://www.compadre-db.org/Page/UserAgreement")
  
 return(dbOut) 
}
