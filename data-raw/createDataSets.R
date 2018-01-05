# Script that creates the subsetted versions of the COM(P)ADRE
# data bases distributed in RCompadre

# This script assumes that you already have the full versions
# of the data bases loaded into R
# Each data set will include ~200 observations + matrices

library(Rcompadre)
library(dplyr)

# Find most of the species mentioned in the vignette
# I don't think we should all of them as that would 
# result in a rather large data set that we distribute
# and CRAN would probably frown upon that.
bonyFish <- subsetDB(comadre, MatrixComposite == "Mean" &
                       Class == "Actinopterygii" &
                       StudyDuration >= 3 &
                       MatrixDimension > 3) %>%
  .$metadata %>%
  .$SpeciesAccepted

Carnivores <- subsetDB(comadre, MatrixComposite == "Mean" &
                         Order == "Carnivora" &
                         MatrixCaptivity == "W" &
                         Lat > 0 &
                         SurvivalIssue < 1 &
                         MatrixSplit == "Divided" &
                         MatrixFec == "Yes") %>%
  .$metadata %>%
  .$SpeciesAccepted



ComadreSpp <- c("Ursus_maritimus", "Pterois volitans",
                bonyFish, Carnivores, "Lepus_americanus")

COMADRE <- subsetDB(comadre, SpeciesAccepted %in% ComadreSpp)

# currently, we have 118 species.
dim(COMADRE$metadata)

# need to add some survival issues for demo purposes
summary(COMADRE$metadata$SurvivalIssue) 

# Solid range of study durations
summary(COMADRE$metadata$StudyDuration)

# good range of matrix dimensions too 
summary(COMADRE$metadata$MatrixDimension)

# Decent range of population #s
summary(COMADRE$metadata$NumberPopulations)

# Going to add in some problem children 
set.seed(5)
idxToAdd <- which(comadre$metadata$SurvivalIssue > 1) %>%
  base::sample(size = 20) %>%
  sort

DbToAdd <- list(metadata = comadre$metadata[idxToAdd, ],
                matrixClass = comadre$matrixClass[idxToAdd],
                mat = comadre$mat[idxToAdd],
                version = comadre$version)

mergeDBs <- function(db1, db2) {
  
  # Probably don't want to combine data bases without matching information
  if(!identical(names(db1), names(db2)) |
     !identical(names(db1$metadata), names(db2$metadata))) {
    stop("Data components do not have identical names. Make sure the metadata \n",
         "in each is identical to other.")
    
  }
  
  out <- list(metadata = rbind(db1$metadata,
                               db2$metadata),
              matrixClass = c(db1$matrixClass,
                              db2$matrixClass),
              mat = c(db1$mat,
                      db2$mat),
              version = db1$version)
  
  # create indexes to check output
  seq1 <- seq_len(dim(db1$metadata)[1])
  seq2 <- seq(max(seq1) + 1, dim(out$metadata)[1], by = 1)
  
  if(!identical(db1$matrixClass, 
                out$matrixClass[seq1]) |
     !identical(db2$matrixClass, 
                out$matrixClass[seq2])) {
    
    # Not sure how to report this as an error other than it being my mistake
       stop("Developer is a bonehead. Email levisc8@gmail.com")
     }
                
  
  out$version$Version <- paste(db1$version$Version,
                               " - subset created on ",
                               format(Sys.time(), 
                                      "%b_%d_%Y"),
                               sep = "")
  
  out$version$DateCreated <- paste(db1$version$DateCreated,
                                   " - subset created on ",
                                   format(Sys.time(), 
                                          "%b_%d_%Y"),
                                   sep = "")
  
  return(out)
}

# Create output data set

Comadre <- mergeDBs(COMADRE, DbToAdd)



