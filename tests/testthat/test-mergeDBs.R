context('mergeDBs')

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

test_that('merge produces correct dimensions', {
  BigDB <- mergeDBs(Compadre, Comadre)
  
  expect_equal(dim(BigDB@metadata), c(341, 47))
  expect_equal(length(BigDB@mat), 341)
  
#  SmallerDB <- mergeDBs(Compadre, ComadreList)
#  expect_equal(dim(SmallerDB@metadata), c(160, 47))
#  expect_equal(ComadreList$mat[[1]]$matA,
#               SmallerDB@mat[[151]]@matA)
  
})

test_that('fails correctly', {
  names(Compadre@metadata)[1] <- 'asfdsa'
  expect_error(mergeDBs(Compadre, Comadre),
              regexp = 'Metadata components do not have identical names')
})