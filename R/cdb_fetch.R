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
#' @param version Optional. The version number of a particular database to be
#'   downloaded e.g. "3.0.0". If this is not included (as default) the latest
#'   versions are downloaded.
#'
#' @param userComment An optional string to enable users to add a comment as an
#'   attribute to the returned data frame. This could be useful for keeping
#'   track of multiple versions of the database. Accessed by
#'   `attributes(db)$comment`.
#'
#' @param quiet A logical argument. If `TRUE` the download message that includes
#'   version information and link to the user agreement is suppressed. Default
#'   is `FALSE`.
#'
#' @param flag Logical argument where `TRUE` will automatically run
#'   \link{cdb_flag} to add logical columns to the metadata to flag potential
#'   problems in the matrix population models. Default is `FALSE`.
#'
#' @note The downloaded databases include a set of attributes accessible with
#'   `attributes`. These include version information and date and time of
#'   creation.
#'
#' @return A CompadreDB object
#'
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @author Patrick M. Barks <patrick.barks@@gmail.com>
#'
#' @family data acquisition
#'
#' @examples
#' \dontrun{
#' # Download latest version of COMPADRE direct from the website
#' compadre <- cdb_fetch("compadre")
#'
#' # Examine the attributes of the downloaded database
#' attributes(compadre)
#'
#' # Download COMPADRE version 3.0.0 direct from the website
#' compadre <- cdb_fetch("compadre", version = "3.0.0")
#'
#' # using file path to downloaded data
#' compadre <- cdb_fetch("data/COMPADRE_v.5.0.1.RData")
#' }
#' @export
cdb_fetch <- function(cdb, version = NULL, flag = FALSE, userComment = NULL,
                      quiet = FALSE) {
  # get url or path
  if (tolower(cdb) == "comadre") {
    if (is.null(version)) {
      path <- url("https://www.compadre-db.org/Data/ComadreDownload")
    } else {
      path <- url(paste0(
        "https://www.compadre-db.org/Data/Download/",
        toupper(cdb), "_", "v.", version, ".RData"
      ))
    }
  } else if (tolower(cdb) == "compadre") {
    if (is.null(version)) {
      path <- url("https://www.compadre-db.org/Data/CompadreDownload")
    } else {
      path <- url(paste0(
        "https://www.compadre-db.org/Data/Download/",
        toupper(cdb), "_", "v.", version, ".RData"
      ))
    }
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

  # optional flag of problematic matrices
  if (flag == TRUE) {
    dbOut <- cdb_flag(dbOut)
  }

  if (!is.null(userComment)) {
    comment(dbOut) <- userComment
  }

  # print db name, version, and release date
  name <- toupper(x)
  version <- Version(dbOut)
  release <- DateCreated(dbOut)

  message(
    "This is ", name, " version ", version, " (release date ", release,
    ")", "\n", "See user agreement at ",
    "https://compadre-db.org/Help/UserAgreement", "\n",
    "See how to cite with `citation(Rcompadre)`", "\n"
  )

  return(dbOut)
}
