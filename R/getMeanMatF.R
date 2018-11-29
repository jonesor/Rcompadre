#' Calculates a population-specific grand mean fecundity matrix for each set of
#' matrices in a COM(P)ADRE database object
#'
#' This function takes a COM(P)ADRE database object and calculates a grand mean
#' fecundity matrix for each unique population (a mean of all
#' population-specific fecundity matrices, including fecundity matrices for
#' which \code{MatrixComposite == 'Mean'}). Here, a unique study population is
#' defined as a unique combination of the metadata columns Authors,
#' YearPublication, DOI.ISBN, SpeciesAuthor, MatrixPopulation, and
#' MatrixDimension. The main purpose of this function is to identify stage
#' classes that are \emph{potentially} reproductive (i.e. the absense of
#' fecundity in a given stage class and year does not necessarily indicate that
#' the stage in question is non-reproductive).
#'
#' @param db A COM(P)ADRE database object
#' 
#' @return Returns a list which contains the mean fecundity matrix associated
#'   with a given row of the database.
#' 
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Julia Jones <juliajones@@biology.sdu.dk>
#' @author Iain Stott <stott@@biolgy.ox.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @examples
#' # print matF associated with row 16 of database
#' Compadre$mat[[16]]
#'
#' # create list of meanMatFs
#' meanF <- getMeanMatF(Compadre)
#'
#' # print meanMatF associated with row 16 of database
#' meanF[[16]]
#' 
#' @export getMeanMatF
getMeanMatF <- function(db) {

  if (!inherits(db, "CompadreDB")) {
    stop("db must be of class CompadreDB. See function asCompadreDB")
  }
  
  # create a unique identifier for each population in the database
  db$PopId <- as.numeric(as.factor(paste(
    db$Authors,
    db$YearPublication,
    db$DOI.ISBN,
    db$SpeciesAuthor,
    db$MatrixPopulation,
    db$MatrixDimension
  )))
  
  # create unique row ID
  db$RowId <- seq_len(nrow(CompadreData(db)))
  
  # extract matFs
  db$matF <- matF(db)
  
  # create vector of unique PopIds, and list of corresponding meanMatFs
  unique_study_pop <- sort(unique(db$PopId))
  unique_mean_mat_F <- lapply(
    unique_study_pop, FUN = meanMatF,
    db_RowId = db$RowId, db_PopId = db$PopId, db_matF = db$matF
  )
  
  # match unique mean matFs to original rows of newdata
  m <- match(db$PopId, unique_study_pop)
  out <- unique_mean_mat_F[m]
  
  return(out)
}



# get mean matrix from list of matrices
mean_list <- function(x) {
  n <- length(x)
  return(Reduce("+", x) / n)
}


# function to return a mean matF given db and PopId
meanMatF <- function(PopIdFocal, db_RowId, db_PopId, db_matF) {
  RowIdFocal <- db_RowId[db_PopId == PopIdFocal]
  return(mean_list(db_matF[RowIdFocal]))
}

