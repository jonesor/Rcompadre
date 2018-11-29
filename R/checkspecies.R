#' Check whether a COM(P)ADRE database contains one or more species of interest
#'
#' This function takes a vector of species names and checks whether those
#' species are represented within a CompadreDB object. It outputs either a data
#' frame depicting the species of interest and whether they occurr in the
#' database (TRUE/FALSE), or, if \code{returnDatabase == TRUE}, a CompadreDB
#' object subset to the species of interest.
#' 
#' @param cdb A CompadreDB object
#' @param species A character vector of binomial species names, with the genus
#'   and specific epithet separated by either an underscore or a space (e.g.
#'   \code{c("Acipenser_fulvescens", "Borrelia_burgdorferi")})
#' @param returnDatabase A logical argument indicating whether a database should
#'   be returned.
#' 
#' @return If \code{returnDatabase == FALSE}, returns a data frame with a column
#'   of species names and a column indicating whether a species occurs in the
#'   database. If \code{returnDatabase == TRUE}, returns a subset of \code{cdb}
#'   containing only those species within argument \code{species}.
#' 
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @author Rob Salguero-Gómez <rob.salguero@@zoo.ox.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @examples
#' species <- c("Primula vulgaris", "Trillium ovatum", "Homo sapiens")
#' cdb_check_species(Compadre, species)
#' CompadreSubset <- cdb_check_species(Compadre, species, returnDatabase = TRUE)
#' 
#' @export cdb_check_species
cdb_check_species <- function(cdb, species, returnDatabase = FALSE) {
  # create dataframe with column for species, and column for whether they are
  #   present in database
  
  if (!inherits(cdb, "CompadreDB")) {
    stop("db must be of class CompadreDB. See function as_cdb")
  }
  
  inDatabase <- vapply(species, findSpecies, db = cdb, FALSE, USE.NAMES = FALSE)
  df <- data.frame(species, inDatabase)
  
  if (returnDatabase == TRUE) {
    ssdb <- cdb[cdb$SpeciesAccepted %in% species,]
    return(ssdb)
  } else {
    return(df)
  }
}


# Utility function
findSpecies <- function(species, db) {
  tolower(species) %in% tolower(gsub('_', ' ', db$SpeciesAccepted))
}
