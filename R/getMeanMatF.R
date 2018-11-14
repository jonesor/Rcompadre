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
#' @param db A COM(P)ADRE database object. Databases will be will be coerced
#'  from the old 'list' format where appropriate (compadre_v4.0.1 and below; 
#' comadre_v2.0.1 and below).
#' 
#' @return Returns a list which contains the mean fecundity matrix associated
#'   with a given row of the database, or NA if there is only a single matrix
#'   from the relevant population within the db.
#' 
#' @author Danny Buss <dlb50@@cam.ac.uk>
#' @author Julia Jones <juliajones@@biology.sdu.dk>
#' @author Iain Stott <stott@@biolgy.ox.ac.uk>
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' 
#' @examples
#' \dontrun{
#' # print set of matrices (A, U, F, C) associated with row 2 of database
#' compadre$mat[[2]]
#'
#' # create list of meanMatFs
#' meanF <- getMeanMatF(compadre)
#'
#' # print meanMatF associated with row 2 of database
#' compadre_with_meanF$mat[[2]]
#' }
#' 
#' @importFrom methods as
#'
#' @export getMeanMatF
#' 
getMeanMatF <- function(db) {

  # convert legacy versions of COM(P)ADRE from class 'list' to 'CompadreData'
  if (class(db) == "list"){
    if( "Animalia" %in% db$metadata$Kingdom ) vlim <- 201
    if( "Plantae" %in% db$metadata$Kingdom ) vlim <- 401
    if (as.numeric(gsub("\\.", "", sub("(\\s.*$)", "", db$version$Version))) <= vlim){
      db <- methods::as(db, "CompadreData")
    }
  }

  newdata <- data(db)
  # create a unique identifier for each population in the database
  newdata$PopId <- as.numeric(as.factor(paste(
    Authors(db),
    YearPublication(db),
    DOI.ISBN(db),
    SpeciesAuthor(db),
    MatrixPopulation(db),
    MatrixDimension(db)
  )))
  
  # subset database to only mean matrices that are divided,
  # and create unique row ID
  ssdb <- subsetDB(db, MatrixSplit %in% "Divided")
  ssnewdata <- data(ssdb)
  ssnewdata$RowId <- seq_len(nrow(ssnewdata)) 
  
  # function to return a mean mean matF given PopId
  meanMatF <- function(PopIdFocal) {
    RowId <- subset(ssnewdata, PopId == PopIdFocal)$RowId
    meanMatFs <- lapply(RowId, function(y) matF(ssdb)[[y]] )
    
    if (length(meanMatFs) == 1) {        # if only one meanMatF for given PopId
      return(NA)
    } else {                             # if multiple meanMatF for given PopId
      meanMatFsSum <- matrix(0,
                             nrow = nrow(meanMatFs[[1]]),
                             ncol = ncol(meanMatFs[[1]]))
      
      for(i in 1:length(meanMatFs)) {
        meanMatFsSum <- meanMatFsSum + meanMatFs[[i]]
      }
      
      return(meanMatFsSum / length(meanMatFs))
    }
  }
  
  # create vector of unique PopIds, and list of corresponding meanMatFs
  unique_study_pop <- sort(unique(newdata$PopId))
  unique_mean_mat_F <- lapply(unique_study_pop, meanMatF)

  # function to return meanMatF corresponding to given row number of db
  appendMeanMatF <- function(i) {
    PopId <- newdata$PopId[i]
    index_mean_mat_F <- which(unique_study_pop == PopId)
    return(unique_mean_mat_F[[index_mean_mat_F]])
  }
  
  # create list of meanMatFs for each row of database
  meanMatList <- lapply(1:nrow(newdata), appendMeanMatF)
  
  return(meanMatList)
}