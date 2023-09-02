#' Calculate a population-specific mean fecundity matrix for each set of
#' matrices in a COM(P)ADRE database
#'
#' @description
#' Takes a CompadreDB object and calculates a grand mean fecundity matrix for
#' each unique population (a mean of all population-specific fecundity matrices,
#' including fecundity matrices for which \code{MatrixComposite == 'Mean'}).
#'
#' Populations are defined based on unique combinations of the columns
#' 'SpeciesAuthor', 'MatrixPopulation', and 'MatrixDimension', (or optionally, a
#' different set of columns supplied by the user).
#'
#' The main purpose of this function is to identify stage classes that are
#' \emph{potentially} reproductive (i.e. the absence of fecundity in a given
#' stage class and year does not necessarily indicate that the stage in question
#' is non-reproductive).
#'
#' @param cdb A CompadreDB object
#' @param columns Vector of column names from which unique populations should be
#'   identified. Defaults to \code{c("SpeciesAuthor", "MatrixPopulation",
#'   "MatrixDimension")}.
#'
#' @return Returns a list of matrices, representing the mean fecundity matrix
#'   associated with each row of the database.
#'
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Julia Jones <juliajones@@biology.sdu.dk>
#' @author Iain Stott <stott@@biology.sdu.dk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#'
#' @family data management
#'
#' @examples
#' # print matF associated with row 16 of database
#' Compadre$mat[[16]]
#'
#' # create list of meanMatFs
#' meanF <- cdb_mean_matF(Compadre)
#'
#' # print meanMatF associated with row 16 of database
#' meanF[[16]]
#'
#' @export cdb_mean_matF
cdb_mean_matF <- function(cdb, columns = c(
                            "SpeciesAuthor",
                            "MatrixPopulation",
                            "MatrixDimension"
                          )) {
  # leave validation to cdb_id

  # create a unique identifier for each population in the database
  cdb$PopId <- cdb_id(cdb, columns)

  # create unique row ID
  cdb$RowId <- seq_len(nrow(cdb@data))

  # extract matFs
  cdb$matF <- matF(cdb)

  # create vector of unique PopIds, and list of corresponding meanMatFs
  unique_study_pop <- sort(unique(cdb$PopId))
  unique_mean_mat_F <- lapply(
    unique_study_pop,
    FUN = meanMatF,
    db_RowId = cdb$RowId, db_PopId = cdb$PopId, db_matF = cdb$matF
  )

  # match unique mean matFs to original rows of newdata
  m <- match(cdb$PopId, unique_study_pop)
  out <- unique_mean_mat_F[m]

  return(out)
}



# function to return a mean matF given db and PopId
meanMatF <- function(PopIdFocal, db_RowId, db_PopId, db_matF) {
  RowIdFocal <- db_RowId[db_PopId == PopIdFocal]
  return(mat_mean(db_matF[RowIdFocal]))
}
