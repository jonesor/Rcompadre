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
#' @param db A COM(P)ADRE database object. Databases will be will be coerced
#'  from the old 'list' format where appropriate (compadre_v4.0.1 and below; 
#' comadre_v2.0.1 and below).
#' @param returnDatabase A logical argument indicating whether a database 
#' should be returned.
#' 
#' @return If returnDatabase = FALSE, \code{checkSpecies} returns a data frame 
#'   with a column of species names and a column indicating whether a species 
#'   occurs in the database. If returnDatabase == TRUE, returns  a subset of db 
#'   containing only those species within argument \code{species}. 
#'   \code{findSpecies} returns TRUE if a species is found in the database, 
#'   FALSE if not, and is called by \code{checkSpecies}.
#' 
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @author Rob Salguero-Gómez <rob.salguero@@zoo.ox.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @examples
#' \dontrun{
#' species <- c("Mammillaria gaumeri", "Euterpe edulis", "Homo sapiens")
#' checkSpecies(species, compadre)
#' compadre_subset <- checkSpecies(species, compadre, returnDatabase = TRUE)
#' }
#' 
#' @export checkSpecies
#' 
checkSpecies <- function(species, db, returnDatabase = FALSE) {
  # create dataframe with column for species, and column for whether they are
  #   present in database
  
  inDatabase <- sapply(species, findSpecies, db = db, USE.NAMES = FALSE)
  df <- data.frame(species, inDatabase)
  
  if (all(df$inDatabase == FALSE)) {
    warning("None of the species were found in the database", call. = FALSE)
  }
  
  if (returnDatabase == TRUE) {
    ssdb <- subsetDB(db, SpeciesAccepted %in% species)
    return(ssdb)
  } else {
    return(df)
  }
}

#' Utility function for checkSpecies
#'
#' @rdname checkSpecies
#' 
#' @return A logical indicating whether the species name is in the 
#' COM(P)ADRE object.
#' 
#' @importFrom methods as
#' 
#' @export findSpecies
findSpecies <- function(species, db) {
  if (class(db) == "list"){
    if( "Animalia" %in% db$metadata$Kingdom ) vlim <- 201
    if( "Plantae" %in% db$metadata$Kingdom ) vlim <- 401
    if (as.numeric(gsub("\\.", "", sub("(\\s.*$)", "", db$version$Version))) <= vlim){
      db <- methods::as(db, "CompadreData")
    }
  }
  tolower(species) %in% tolower(gsub('_', ' ', SpeciesAccepted(db)))
}
