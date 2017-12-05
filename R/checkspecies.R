#' Check whether a COM(P)ADRE database contains one or more species of interest
#' 
#' This function takes a vector of species names and checks whether those
#' species are represented within a COM(P)ADRE database. It outputs either a
#' data frame depicting the species of interest and whether they occurr in the
#' database (TRUE/FALSE), or, if returnDatabase == TRUE, a COM(P)ADRE database
#' object subset to the species of interest.
#' 
#' @param species A character vector of binomial species names, with the
#'   genus and specific epithet separated by either an underscore or a space (
#'   e.g. c("Acipenser_fulvescens", "Borrelia_burgdorferi"))
#' @param db A COM(P)ADRE database.
#' @return If returnDatabase = FALSE, returns a data frame with a column of
#'   species names and a column indicating whether a species occurs in the
#'   database. If returnDatabase == TRUE, returns  a subset of db containing
#'   only those species within argument \code{species}
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @author Rob Salguero-Gómez <rob.salguero@@zoo.ox.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @examples
#' \dontrun{
#' species <- c("Mammillaria gaumeri", "Euterpe edulis", "Homo sapiens")
#' checkSpecies(species, compadre)
#' compadre_subset <- checkSpecies(species, compadre, returnDatabase = TRUE)
#' }
#' @export checkSpecies
checkSpecies <- function(species, db, returnDatabase = FALSE){
  
  # create dataframe with column for species, and column for whether they are
  #   present in database
  findSpecies <- function(x) tolower(x) %in% tolower(gsub('_', ' ', db$metadata$SpeciesAccepted))
  inDatabase <- sapply(species, findSpecies, USE.NAMES = FALSE)
  df <- data.frame(species, inDatabase)
  
  if (all(df$inDatabase == FALSE)) {
    warning("None of the species were found in the database", call. = FALSE)
  }
  
  if(returnDatabase == TRUE) {
    ssdb <- subsetDB(db, SpeciesAccepted %in% species)
    return(ssdb)
  } else {
    return(df)
  }
}