#' Appends the mean fecundity matrix for each unique population in a new database (only individuals matrices)
#' 
#' This function takes a compadre or comadre only and filters the database for only individual matrices
#' It will then append the mean fecundity matrix for each population to the database
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param db The COMPADRE or COMADRE database object.
#' @return Returns a subset of the database with only individual matrices the same structure as the COMPADRE
#' or COMADRE database object, but which also contains the mean fecundity matrix (MatFMu).
#' @note %% ~~further notes~~
#' @author Danny Buss <dlb50@cam.ac.uk>
#' Julia Jones <juliajones@biology.sdu.dk>
#' Iain Stott <stott@@biolgy.ox.ac.uk>
#' @examples
#' 
#' @export append_MatFmu
#DANNY HAS DONE A ROB AND NEEDS TO FIX THIS - it works but appears to put things back together wrong
append_MatFmu <- function(db){
  #subset of database with only Individual divisible matrices and a sep one for mean matrices
  ssdb<-subsetDB(db, MatrixComposite=="Individual" & MatrixSplit=="Divided")
#  ssdb<-subsetDB(db, MatrixComposite=="Individual")
  ssdb_mean<-subsetDB(db, MatrixComposite=="Mean" & MatrixSplit=="Divided")
#  ssdb_mean<-subsetDB(db, MatrixComposite=="Mean")
  len<-length(ssdb$metadata$SpeciesAccepted)
  #flatten DBs 
  ssdb2<-convert2flat(ssdb)
  ssdb2_mean<-convert2flat(ssdb_mean)
  
  #create a unique identifier for each population in each database
  popname<-as.character(paste(ssdb$metadata$SpeciesAuthor,ssdb$metadata$MatrixPopulation, ssdb$metadata$MatrixDimension))
  popname<-gsub(" ","",popname)
  popname<-gsub("_","",popname)
  ssdb2$popname<-popname
  ssdb2$ID<-rep(1:length(popname))

  popname_mean<-as.character(paste(ssdb_mean$metadata$SpeciesAuthor,ssdb_mean$metadata$MatrixPopulation, ssdb_mean$metadata$MatrixDimension))
  popname_mean<-gsub(" ","",popname_mean)
  popname_mean<-gsub("_","",popname_mean)
  ssdb2_mean<-data.frame(popname_mean,ssdb2_mean$matrixF)
  #rename columns
  names(ssdb2_mean)<-c("popname","meanF")
  #Remove complete duplicates (popname & meanF)
  ssdb2_mean$meanF<-as.character(ssdb2_mean$meanF)
  ssdb2_mean<-ssdb2_mean[!duplicated(ssdb2_mean),]

  # randomly sample mean fecundity matrices for duplicated populations function, taken from:
  #https://amywhiteheadresearch.wordpress.com/2013/01/22/randomly-deleting-duplicate-rows-from-a-dataframe-2/
  duplicated.random = function(x, incomparables = FALSE, ...) 
  { 
    if ( is.vector(x) ) 
    { 
      permutation = sample(length(x)) 
      x.perm      = x[permutation] 
      result.perm = duplicated(x.perm, incomparables, ...) 
      result      = result.perm[order(permutation)] 
      return(result) 
    } 
    else if ( is.matrix(x) ) 
    { 
      permutation = sample(nrow(x)) 
      x.perm      = x[permutation,] 
      result.perm = duplicated(x.perm, incomparables, ...) 
      result      = result.perm[order(permutation)] 
      return(result) 
    } 
    else 
    { 
      stop(paste("duplicated.random() only supports vectors", 
                 "matrices for now.")) 
    } 
  }
  
  #Randomly sample and then remove the randomly selected duplicates
  ssdb2_mean$popnametemp<-as.numeric(ssdb2_mean$popname)
  ssdb2_mean$dupe<-duplicated.random(ssdb2_mean$popnametemp)
  ssdb2_mean<-ssdb2_mean[!ssdb2_mean$dupe==TRUE,]
  ssdb2_mean<-ssdb2_mean[,c(1:2)]

  #Merge by popname
  ssdb2_mean$meanF<-as.character(ssdb2_mean$meanF)
  ssdb2_mean$popname<-as.character(ssdb2_mean$popname)
  ssdb2$popname<-as.character(ssdb2$popname)
  temp<-merge(ssdb2,ssdb2_mean,by="popname",all.x=T)

  #Populate matFmu matrices by string2matrix meanF matrices
  out<-lapply(temp$meanF, function(x) stringtomatrix(x))
  #WOULD BE GOOD TO ADD A WARNING HERE THAT CHECKS WHETHER THE NEW FECUNDITY MATRIX IS THE SAME DIMENSIONS AS THE MATF

  #After aggregation append empty matrices onto individual dataframe (ssdb)
  ###Appends matFMu into mat within ssdb - Iain Stott wrote this bit
  ssdb$mat<-mapply(FUN = function(X,i){c(X,matFmu = out[i])}, X = ssdb$mat, i = 1:length(ssdb$mat), SIMPLIFY = FALSE)
  return(ssdb)
}