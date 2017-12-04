#' Check if the database contains any of 'my' species
#' 
#' This function takes a list of species and checks whether they are in the database
#' and outputs the species list in dataframe with a True or False Column. If True, your species have been found.
#' Verbose = T will use the subsetDB function
#' and create a subset of the Compadre Object containing only your species. 
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param sp_list A character vector of the users desired species. This is assumed to be
#' with an underscore between genus and binomial, for example: sp_list<-c("Acipenser_fulvescens","Borrelia_burgdorferi","Falco_peregrinus")
#' @param db The COMPADRE or COMADRE database object.
#' @return Returns a subset of the database when verbose=T, with the same structure as the COMPADRE
#' or COMADRE database object, but which only contains data associated with the species in sp_list object.
#' @note %% ~~further notes~~
#' @author Danny Buss <dlb50@cam.ac.uk>
#' Owen R. Jones <jones@@biology.sdu.dk>
#' Rob Salguero-Goómez <rob.salguero@@zoo.ox.ac.uk>
#' @examples
#' 
#' \dontrun{
#' ssData <- subsetDB(compadre, SpeciesAccepted =="")
#' ssData <- subsetDB(compadre, SpeciesAccepted==sp_list)
#' ssData<-subsetDB(comadre, SpeciesAccepted =="Acipenser_fulvescens")
#' ssData <- subsetDB(comadre, SpeciesAccepted==sp_list)
#' }
#' 
#' @export checkspecies
checkspecies <- function(sp_list, db, verbose = FALSE){
# 1. Create the True, False dataframe with species list and output this
  db<-db
  sp<-sp_list
  out<-sapply(sp_list, function(x) x %in% db$metadata$SpeciesAccepted,USE.NAMES =FALSE)
    df<-data.frame(sp,out)
    names(df)[1]<-"Species"
    names(df)[2]<-"InDatabase"
    return(df)
    if(verbose == TRUE){
      ssdb<-subsetDB(db, SpeciesAccepted==sp_list)
      return(ssdb)
    }
}
# 2. If verbose = T, use the subsetDB function to create a subset of COMPADREDB. If verbose = F, only return species list, output two things?

# 3. Need to add - if none of your species are in compadre, return message "no species found in database"
#