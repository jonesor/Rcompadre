#' Appends a population-specific mean fecundity matrix to each set of matrices
#' in a COM(P)ADRE database object
#'
#' This function takes a COM(P)ADRE database object, calculates a mean fecundity
#' matrix for each unique population (a mean of all population-specific
#' fecundity matrices for which MatrixComposite == "Mean"), and appends the
#' relevant mean fecundity matrix to each original set of matrices in the
#' database. For the purpose of this function, a unique study population is
#' defined as a unique combination of the metadata columns Authors,
#' YearPublication, DOI.ISBN, SpeciesAuthor, MatrixPopulation, and
#' MatrixDimension.
#' 
#' @param db A COM(P)ADRE database object. Database versions <=4.0.1 will be coerced
#'  from class 'list'.
#' @return Returns db, but with an extra entry in slot `mat` which contains the
#'   mean fecundity matrix associated with a given row of the database, or NA if
#'   there are no mean matrices within the db from the relevant population.
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Julia Jones <juliajones@@biology.sdu.dk>
#' @author Iain Stott <stott@@biology.ox.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @examples
#' \dontrun{
#' # print set of matrices (A, U, F, C) associated with row 2 of database
#' compadre$mat[[2]]
#' 
#' # append meanMatFs to database object
#' compadre_with_meanF <- appendMeanMatF(compadre)
#' 
#' # print matrices associated with row 2 of database, including meanMatF
#' compadre_with_meanF$mat[[2]]
#' }
#' @export
#' @importFrom rlang .data
appendMeanMatF <- function(db) {
  # convert legacy versions of COM(P)ADRE from class 'list' to 'CompadreData'
  if (class(db) == "list"){
    if (as.numeric(gsub("\\.", "", sub("(\\s.*$)", "", db$version$Version))) <= 401){
      db <- as(db, "CompadreData")
    }
  }
  
  # create a unique identifier for each population in the database
  db@metadata$PopId <- as.numeric(as.factor(paste(
    db@metadata$Authors,
    db@metadata$YearPublication,
    db@metadata$DOI.ISBN,
    db@metadata$SpeciesAuthor,
    db@metadata$MatrixPopulation,
    db@metadata$MatrixDimension
  )))
  
  # subset database to only mean matrices that are divided,
  # and create unique row ID
  ssdb_mean <- subsetDB(db, MatrixComposite == "Mean" &
                          MatrixSplit == "Divided")
  
  ssdb_mean@metadata$RowId <- seq_len(nrow(ssdb_mean@metadata)) 
  
  # function to return a mean mean matF given PopId
  meanMatF <- function(PopIdFocal) {
    RowId <- subset(ssdb_mean@metadata, PopId == PopIdFocal)$RowId
    meanMatFs <- lapply(RowId, function(y) ssdb_mean@mat[[y]]@matF)
    
    if (length(meanMatFs) == 0) {        # if no meanMatF for given PopId
      return(NA)
    } else if (length(meanMatFs) == 1) { # if one meanMatF for given PopId
      return(meanMatFs[[1]])
    } else {                             # if multiple meanMatF for given PopId
      return(mean(meanMatFs))
    }
  }
  
  # create vector of unique PopIds, and list of corresponding meanMatFs
  unique_study_pop <- sort(unique(db@metadata$PopId))
  unique_mean_mat_F <- lapply(unique_study_pop, meanMatF)
  
  # function to append meanMatF to slot mat, given row number of db
  appendMeanMatF <- function(i) {
    PopId <- db@metadata$PopId[i]
    index_mean_mat_F <- which(unique_study_pop == PopId)
    out <- db@mat[[i]]
    out@matFmean <- unique_mean_mat_F[[index_mean_mat_F]]  ## FAILS here because not valid slot name
    return(out)
  }
  
  # append meanMatFs for each row of database
  db@mat <- lapply(1:nrow(db@metadata), appendMeanMatF)
  
  return(db)
}
