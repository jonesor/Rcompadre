context('convertLegacyDB')

# Load in data sets and create a smaller version
# of our legacy system. Using 10 from COMADRE
# for that purpose
data(Compadre)
data(Comadre)

matrixClass <- list()
mat <- list()

for(i in 1:10) {
  matrixClass[[i]] <- Comadre@mat[[i]]@matrixClass
  mat[[i]] <- list(matA = Comadre@mat[[i]]@matA,
                   matU = Comadre@mat[[i]]@matU,
                   matF = Comadre@mat[[i]]@matF,
                   matC = Comadre@mat[[i]]@matC)
}

ComadreList <- list(metadata = Comadre@metadata[1:10, ],
                    matrixClass = matrixClass,
                    mat = mat,
                    version = Comadre@version)

test_that('convertLegacyDB can utilize newest old version', {
  db <- convertLegacyDB(ComadreList) # convert a legacy version
  db2 <- convertLegacyDB(Compadre) # but do nothing with new class
  
  expect_true(inherits(db, 'CompadreData'))
  expect_true(inherits(db2, 'CompadreData'))
  expect_true(identical(db2, Compadre))
  
  expect_equal(dim(db@metadata), c(10, 47))
  
  expect_equal(dim(db@mat[[1]]@matA),
               dim(ComadreList$mat[[1]]$matA))
  
})

test_that('convertLegacyDB fails properly', {
  names(ComadreList)[2] <- 'NewName'
  
  expect_error(convertLegacyDB(ComadreList),
               regexp = "'CompadreData' objects require at least the following")
  
})
