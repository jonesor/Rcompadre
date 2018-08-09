# Script that creates the subsetted versions of the COM(P)ADRE
# data bases distributed in RCompadre


library(RcompadreDev)
library(dplyr)

# Store package working directory, load most recent
# Com(p)adre versions, then reset to package working directory
oldwd <- getwd()

setwd('C:/Users/sl13sise/Dropbox/sApropos project/DemogData')
load('COMPADRE_v.4.0.1.RData')
load('COMADRE_v.X.X.X.2.RData')
setwd(oldwd)

NegativeMatrices <- logical(length(compadre$mat))
for(i in seq_len(dim(compadre$metadata)[1])) {
  NegativeMatrices[i] <- any(compadre$mat[[i]]$matA < 0 |
                               compadre$mat[[i]]$matU < 0,
                             na.rm = TRUE)
}


compadre <- list(metadata = compadre$metadata[!NegativeMatrices, ],
                 matrixClass = compadre$matrixClass[!NegativeMatrices],
                 mat = compadre$mat[!NegativeMatrices],
                 version = compadre$version)

# Find most of the species mentioned in the vignette
# I don't think we should use all of them as that would 
# result in a rather large data set being distributed
# and CRAN may frown upon that.

if(!inherits(comadre, 'CompadreData')) {
  comadre <- RcompadreDev:::asCompadreData(comadre)
  
}
  
bonyFish <- subsetDB(comadre, MatrixComposite == "Mean" &
                       Class == "Actinopterygii" &
                       StudyDuration >= 3 &
                       MatrixDimension > 3) %>%
  .@metadata %>%
  .$SpeciesAccepted

Carnivores <- subsetDB(comadre, MatrixComposite == "Mean" &
                         Order == "Carnivora" &
                         MatrixCaptivity == "W" &
                         Lat > 0 &
                         SurvivalIssue < 1 &
                         MatrixSplit == "Divided" &
                         MatrixFec == "Yes") %>%
  .@metadata %>%
  .$SpeciesAccepted



ComadreSpp <- c("Ursus_maritimus", "Pterois volitans",
                bonyFish, Carnivores, "Lepus_americanus")

COMADRE <- subsetDB(comadre, SpeciesAccepted %in% ComadreSpp)

# currently, we have 118 species.
dim(COMADRE@metadata)

# need to add some survival issues for demo purposes
summary(COMADRE@metadata$SurvivalIssue) 

# Solid range of study durations
summary(COMADRE@metadata$StudyDuration)

# good range of matrix dimensions too 
summary(COMADRE@metadata$MatrixDimension)

# Decent range of population #s
summary(COMADRE@metadata$NumberPopulations)

# But a bad range of Continents! Thanks to Patrick Barks for pointing
# that out
summary(COMADRE@metadata$Continent)


# Going to add in some problem children 


DbToSample <- subsetDB(comadre, 
                    SurvivalIssue > 2 |
                      Continent == 'Africa' | 
                      Continent == 'Asia' |
                      Continent == 'Australia' |
                      Continent == 'S America')

set.seed(20)
AddToDBIDX <- base::sample(rownames(DbToSample@metadata),
                           size = 40,
                           replace = FALSE) %>%
  sort

DbToAdd <- subsetDB(DbToSample, 
                    as.numeric(rownames(DbToSample@metadata)) %in% AddToDBIDX)

summary(DbToAdd@metadata$Continent)
summary(DbToAdd@metadata$SurvivalIssue)

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
  base::sample(size = 40) %>%
  sort

DbToAdd2 <- list(metadata = compadre$metadata[idxToAdd2, ],
                matrixClass = compadre$matrixClass[idxToAdd2],
                mat = compadre$mat[idxToAdd2],
                version = compadre$version)



# Create output data set
Comadre <- mergeDBs(COMADRE, DbToAdd) 
Compadre <- mergeDBs(COMPADRE, DbToAdd2) 

# do some final checks to make sure I actually added
# what I intended to add
Comadre@version
dim(Comadre@metadata)

Compadre@version
dim(Compadre@metadata)

summary(Comadre@metadata$SurvivalIssue) 
summary(Comadre@metadata$StudyDuration)
summary(Comadre@metadata$MatrixDimension)
summary(Comadre@metadata$NumberPopulations)
summary(Comadre@metadata$Continent)

summary(Compadre@metadata$SurvivalIssue) 
summary(Compadre@metadata$StudyDuration)
summary(Compadre@metadata$MatrixDimension)
summary(Compadre@metadata$NumberPopulations)
summary(Compadre@metadata$Continent)

# Write the files into the data folder
devtools::use_data(Comadre,
                   Compadre,
                   overwrite = TRUE)


