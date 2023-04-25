#' Check whether a COM(P)ADRE database contains one or more species of interest
#'
#' Takes a vector of species names and checks whether those species are
#' represented within a CompadreDB object. It outputs either a data frame
#' depicting the species of interest and whether they occur in the database
#' (TRUE/FALSE), or, if \code{return_db == TRUE}, a CompadreDB object subset to
#' the species of interest.
#'
#' @param cdb A CompadreDB object
#' @param species Character vector of binomial species names, with the genus and
#'   specific epithet separated by either an underscore or a space (e.g.
#'   \code{c("Acipenser_fulvescens", "Borrelia_burgdorferi")})
#' @param return_db Logical argument indicating whether a database should be
#'   returned
#'
#' @return If \code{return_db == FALSE}, returns a data frame with a column of
#'   species names and a column indicating whether a species occurs in the
#'   database. If \code{return_db == TRUE}, returns a subset of \code{cdb}
#'   containing only those species within argument \code{species}.
#'
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @author Rob Salguero-Gómez <rob.salguero@@zoo.ox.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#'
#' @family data checking
#'
#' @examples
#' species <- c("Primula vulgaris", "Trillium ovatum", "Homo sapiens")
#' cdb_check_species(Compadre, species)
#' CompadreSubset <- cdb_check_species(Compadre, species, return_db = TRUE)
#'
#' @export cdb_check_species
cdb_check_species <- function(cdb, species, return_db = FALSE) {
  if (!inherits(cdb, "CompadreDB")) {
    stop("cdb must be of class CompadreDB. See function as_cdb")
  }

  in_db <- vapply(species, findSpecies, db = cdb, FALSE, USE.NAMES = FALSE)
  df <- data.frame(species, in_db)

  if (return_db == TRUE) {
    return(cdb[cdb$SpeciesAccepted %in% species, ])
  } else {
    return(df)
  }
}


# Utility function
findSpecies <- function(species, db) {
  tolower(species) %in% tolower(gsub("_", " ", db$SpeciesAccepted))
}
