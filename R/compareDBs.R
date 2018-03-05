#' A function to compare two COMPADRE/COMADRE database versions
#' 
#' A function to compare two COMPADRE/COMADRE database versions
#' 
#' 
#' @param db1,db2 COM(P)ADRE database objects to compare. Databases will be 
#' coerced from the old 'list' format where appropriate (compadre_v4.0.1 and 
#' below; comadre_v2.0.1 and below).
#' @param verbose A logical argument indicating whether or not to return lots
#' of detail.
#'
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#'
#' @details \code{compareDBs} is preferred, but \code{dbCompare} is provided 
#' for legacy purposes.
#' 
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' 
#' @keywords manip attribute
#' 
#' @examples
#' \dontrun{
#' 	compareDBs(comadreSubset,comadre,verbose = TRUE)
#' }
#' 
#' @importFrom methods as
#' @export compareDBs
#' 
compareDBs <- function(db1, db2, verbose = FALSE){ 
  # convert legacy versions of COM(P)ADRE from class 'list' to 'CompadreData'
  # convert legacy versions of COM(P)ADRE from class 'list' to 'CompadreData'
  if (class(db1) == "list"){
    if( "Animalia" %in% db1$metadata$Kingdom ) vlim <- 201
    if( "Plantae" %in% db1$metadata$Kingdom ) vlim <- 401
    if (as.numeric(gsub("\\.", "", sub("(\\s.*$)", "", db1$version$Version))) <= vlim){
      db1 <- methods::as(db1, "CompadreData")
    }
  }
  if (class(db2) == "list"){
    if( "Animalia" %in% db2$metadata$Kingdom ) vlim <- 201
    if( "Plantae" %in% db2$metadata$Kingdom ) vlim <- 401
    if (as.numeric(gsub("\\.", "", sub("(\\s.*$)", "", db2$version$Version))) <= vlim){
      db2 <- methods::as(db2, "CompadreData")
    }
  }
  
  #Quick summary
  cat("Quick Summary\n")
  
  #File 1
  uniqueSource1 <- unique(paste(db1@metadata$Authors, " (",
                                db1@metadata$YearPublication, ") ",
                                db1@metadata$Journal, sep = ""))                     
  db1@metadata$binomial <- db1@metadata$SpeciesAccepted
  
  cat(paste("File-1 contains the demographic and associated data from ", 
            length(uniqueSource1), " source papers, corresponding to ",
            length(unique(db1@metadata$binomial))," accepted species, and ",
            nrow(db1@metadata), " matrices.\n\n",sep=""))
  
  #File 2
  uniqueSource2 <- unique(paste(db2@metadata$Authors, " (",
                                db2@metadata$YearPublication, ") ",
                                db2@metadata$Journal, sep = ""))                      
  db2@metadata$binomial <- db2@metadata$SpeciesAccepted
  
  cat(paste("File-2 contains the demographic and associated data for ", 
            length(uniqueSource2), " source papers, corresponding to ",
            length(unique(db2@metadata$binomial))," accepted species, and ",
            nrow(db2@metadata), " matrices.\n\n",sep=""))
  
  if(verbose == TRUE){
    cat("Detailed summary\n")
    
    #Accepted species in File 1 that are not in File 2
    sp1 <- unique(db1@metadata$binomial)
    sp2 <- unique(db2@metadata$binomial)
    
    cat("Number of accepted species in File 1, based on latin binomial\n")
    print(length(sp1))
    
    cat("Number of accepted species in File 2, based on latin binomial\n")
    print(length(sp2))
    
    cat("Accepted species in File 1 that are not in File 2 (based on latin binomial)\n")
    print(sp1[which(!sp1%in%sp2)])
    
    cat("Accepted species in File 2 that are not in File 1 (based on latin binomial)\n")
    print(sp2[which(!sp2%in%sp1)])
    
    #Get unique author species for both files
    asp1 <- unique(db1@metadata$SpeciesAuthor)
    asp2 <- unique(db2@metadata$SpeciesAuthor)
    
    cat("Number of unique study-species combinations in File 1\n")
    print(length(asp1))
    
    cat("Number of unique study-species combinations in File 2\n")
    print(length(asp2))
    
    #cat("Study-species in File 1 that are not in File 2\n")
    #print(asp1[which(!asp1%in%asp2)])
    
    #cat("Study-species in File 2 that are not in File 1\n")
    #print(asp2[which(!asp2%in%asp1)])
    
    cat("\n\nSource papers in File 2 that are not in File 1\n")
    print(sort(uniqueSource2[which(!uniqueSource2%in%uniqueSource1)]))
    
    cat("\n\nSource papers in File 1 that are not in File 2\n")
    print(sort(uniqueSource1[which(!uniqueSource1%in%uniqueSource2)]))
    
    
    cat("See the User Guide for definitiions\n")
  }
}

#' @rdname compareDBs
dbCompare <- function(db1, db2, verbose = FALSE) { 
  compareDBs(db1, db2, verbose)
}