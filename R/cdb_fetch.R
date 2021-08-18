#' Fetch the COM(P)ADRE database from compadre-db.org or a local file
#'
#' Fetches the current version of a COM(P)ADRE database from
#' \url{https://compadre-db.org}, or load any version stored in a local .RData
#' file.
#'
#' @param cdb Either "comadre" or "compadre" (case insensitive) to fetch the
#'   most recent database from \url{https://compadre-db.org}, or a path to an
#'   existing COMPADRE database (i.e. .RData file) stored on the local machine.
#' 
#' @param flag Logical argument where `TRUE` will automatically run
#'   \link{cdb_flag} to add logical columns to the metadata to flag potential
#'   problems in the matrix population models. Default is `FALSE`.
#' 
#' @return A CompadreDB object
#' 
#' @author Patrick M. Barks <patrick.barks@@gmail.com>
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' 
#' @family data acquisition
#' 
#' @examples
#' \dontrun{
#' #Download direct from the COMPADRE website
#' compadre <- cdb_fetch("compadre")
#' 
#' #using file path to downloaded data
#' compadre <- cdb_fetch("data/COMPADRE_v.5.0.1.RData") 
#' }
#' @export
cdb_fetch <- function(cdb, flag = FALSE) {
  
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
  
  #optional flag of problematic matrices
  if(flag == TRUE){
    dbOut <- cdb_flag(dbOut)
  }
  
  # print db name, version, and release date
  name <- toupper(x)
  version <- Version(dbOut)
  release <- DateCreated(dbOut)
  
  message("This is ", name, " version ", version, " (release date ", release,
          ")", "\n", "See user agreement at ",
          "https://compadre-db.org/Help/UserAgreement","\n",
          "See how to cite at ",
          "https://compadre-db.org/Help/HowToCite")
  
 return(dbOut) 
}
