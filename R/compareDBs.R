#' Compare two versions or subsets of a COM(P)ADRE database
#' 
#' Prints a summary of the differences between two CompadreDB objects, including
#' the number of species, studies, and matrices in each. If argument
#' \code{verbose == TRUE}, additionally prints a list of the species and studies
#' that are present in one database but not the other.
#' 
#' @param db1,db2 CompadreDB objects to compare
#' @param verbose A logical argument indicating whether or not to return lots
#' of detail
#' 
#' @return NULL (output is printed rather than returned)
#'
#' @details \code{compareDBs} is preferred, but \code{dbCompare} is provided 
#' for legacy purposes.
#' 
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#' 
#' @examples
#' Compadre1 <- subset(Compadre, Continent == "Asia")
#' Compadre2 <- subset(Compadre, Continent == "Africa")
#' 
#' compareDBs(Compadre1, Compadre2)
#' 
#' @export compareDBs
compareDBs <- function(db1, db2, verbose = FALSE){ 

  if (!inherits(db1, "CompadreDB") | !inherits(db2, "CompadreDB")) {
    stop("dbs must be of class CompadreDB. See function asCompadreDB")
  }

  #Quick summary
  cat("Quick Summary...\n\n")
  
  #File 1
  cat(paste("DB1 contains data for:\n", 
            NumberStudies(db1), " source papers\n",
            NumberAcceptedSpecies(db1)," accepted species\n",
            NumberMatrices(db1), " matrices\n\n", sep = ""))
  
  #File 2
  cat(paste("DB2 contains data for:\n", 
            NumberStudies(db2), " source papers\n",
            NumberAcceptedSpecies(db2)," accepted species\n",
            NumberMatrices(db2), " matrices\n\n", sep = ""))
  
  if(verbose == TRUE){
    cat("Detailed summary...\n\n")
    
    #Accepted species in File 1 that are not in File 2
    sp1 <- unique(db1$SpeciesAccepted)
    sp2 <- unique(db2$SpeciesAccepted)
    
    cat("Species in DB1 not in DB2:\n")
    print(sp1[which(!sp1%in%sp2)])
    
    cat("Species in DB2 not in DB1:\n")
    print(sp2[which(!sp2%in%sp1)])
    
    #Get unique author species for both files
    #asp1 <- unique(SpeciesAuthor(db1))
    #asp2 <- unique(SpeciesAuthor(db2))
    
    #cat("Number of study-species combinations in DB1\n")
    #print(length(asp1))
    
    #cat("Number of study-species combinations in DB2\n")
    #print(length(asp2))
    
    #cat("Study-species in DB1 that are not in DB2\n")
    #print(asp1[which(!asp1%in%asp2)])
    
    #cat("Study-species in DB2 that are not in DB1\n")
    #print(asp2[which(!asp2%in%asp1)])

    uniqueSource1 <- unique(paste(db1$Authors, " (",
                                  db1$YearPublication, ") ",
                                  db1$Journal, sep = ""))

    uniqueSource2 <- unique(paste(db2$Authors, " (",
                                  db2$YearPublication, ") ",
                                  db2$Journal, sep = ""))

    cat("\n\nSource papers in DB2 not in DB1\n")
    print(sort(uniqueSource2[which(!uniqueSource2%in%uniqueSource1)]))
    
    cat("\n\nSource papers in DB1 not in DB2\n")
    print(sort(uniqueSource1[which(!uniqueSource1%in%uniqueSource2)]))
    
    cat("See the User Guide for definitions\n")
  }
}

#' @rdname compareDBs
dbCompare <- function(db1, db2, verbose = FALSE) { 
  compareDBs(db1, db2, verbose)
}
