# Script that creates the subsetted versions of the COM(P)ADRE
# data bases distributed in RCompadre

# This script assumes that you already have the full versions
# of the data bases loaded into R
# Each data set will include ~150 observations + matrices

library(Rcompadre)
library(dplyr)

# Find most of the species mentioned in the vignette
# I don't think we should use all of them as that would 
# result in a rather large data set being distributed
# and CRAN may frown upon that.
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
  # If the user hasn't used subset DB to create the smaller versions,
  # then add in that information
  if(!grepl('subset created', out$version$Version)) {
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
  }
           
  out$version$NumberAcceptedSpecies <- length(unique(out$metadata$SpeciesAccepted))
  out$version$NumberStudies <- length(unique(out$metadata$SpeciesAuthor))
  out$version$NumberMatrices <- length(out$mat)
  
  return(out)
}

# Now, the same for Compadre! I like succulents and trees
# so I'll start with those

set.seed(5)
starterIDX <- which(compadre$metadata$OrganismType == 'Succulent' &
                      compadre$metadata$SurvivalIssue < 1 |
                      compadre$metadata$OrganismType == 'Tree' &
                      compadre$metadata$SurvivalIssue < 1) %>%
  base::sample(size = 110) %>%
  sort()

COMPADRE <- list(metadata = compadre$metadata[starterIDX, ],
                  matrixClass = compadre$matrixClass[starterIDX],
                  mat = compadre$mat[starterIDX],
                  version = compadre$version)

dim(COMPADRE$metadata)

# need to add some survival issues for demo purposes
summary(COMPADRE$metadata$SurvivalIssue) 

# Solid range of study durations
summary(COMPADRE$metadata$StudyDuration)

# good range of matrix dimensions too 
summary(COMPADRE$metadata$MatrixDimension)

# Decent range of population #s
summary(COMPADRE$metadata$NumberPopulations)

# I'll also add a couple monocots
summary(COMPADRE$metadata$DicotMonoc)

set.seed(7)
idxToAdd2 <- which(compadre$metadata$SurvivalIssue > 2) %>%
  base::sample(size = 20) %>%
  sort

DbToAdd2 <- list(metadata = compadre$metadata[idxToAdd2, ],
                matrixClass = compadre$matrixClass[idxToAdd2],
                mat = compadre$mat[idxToAdd2],
                version = compadre$version)



# Create output data set
Comadre <- mergeDBs(COMADRE, DbToAdd) %>% Rcompadre:::asCompadreData()
Compadre <- mergeDBs(COMPADRE, DbToAdd2) %>% Rcompadre:::asCompadreData()

# do some quick checks to make sure nothing got all cattywompus
Comadre@version
dim(Comadre@metadata)

Compadre@version
dim(Compadre@metadata)

# Write the files into the data folder
devtools::use_data(Comadre,
                   Compadre,
                   overwrite = TRUE)


