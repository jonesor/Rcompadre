
# Script that creates the subsetted versions of the COM(P)ADRE
# data bases distributed in RCompadre


# load RcompadreDev
devtools::load_all()

# load dbs
load('~/COMADRE_v.X.X.X.RData')
load('~/COMPADRE_v.X.X.X.RData')

# convert to CompadreDB
Compadre <- asCompadreDB(compadre)
Comadre <- asCompadreDB(comadre)

# random 150 matrices
Compadre <- Compadre[sample(seq_along(Compadre$SpeciesAccepted), 150),]
Comadre <- Comadre[sample(seq_along(Comadre$SpeciesAccepted), 150),]

# Write the files into the data folder
usethis::use_data(Comadre,
                  Compadre,
                  overwrite = TRUE)

