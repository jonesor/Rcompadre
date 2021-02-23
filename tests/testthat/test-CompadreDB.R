context("CompadreDB")

test_that("CompadreDB works correctly", {
  
  db1 <- db2 <- db3 <- db4 <- as_cdb(CompadreLegacy)
  
  expect_true(validCompadreDB(db1))
  expect_output(print(db1))
  
  # missing mat column
  db2@data <- db2@data[,5:10]
  expect_false(isTRUE(validCompadreDB(db2)))
  
  # mat not a list-column
  db3@data$mat <- 1L
  expect_false(isTRUE(validCompadreDB(db3)))
  
  # mat a list-column but without CompadreMat objects
  db4@data$mat <- CompadreLegacy$mat
  expect_false(isTRUE(validCompadreDB(db4)))
  
  # $ and $<-
  expect_equal(Compadre$SpeciesAuthor, Compadre@data$SpeciesAuthor)
  
  db1 <- Compadre
  db1$ones <- 1L
  expect_true(ncol(db1@data) == ncol(Compadre@data) + 1)
  expect_true(all(db1$ones == 1L))
  
  expect_error(Compadre$mat <- 1L)
  
  # [[ and [[<-
  expect_equal(Compadre[["SpeciesAuthor"]], Compadre@data$SpeciesAuthor)
  
  db1 <- Compadre
  db1[["ones"]] <- 1L
  expect_true(ncol(db1@data) == ncol(Compadre@data) + 1)
  expect_true(all(db1$ones == 1L))
  
  expect_error(Compadre[["mat"]] <- 1L)
  
  # accessors
  expect_s3_class(CompadreData(Compadre), "data.frame")
  expect_type(VersionData(Compadre), "list")
  expect_length(Version(Compadre), 1)
  expect_length(DateCreated(Compadre), 1)
  expect_length(DateCreated(Compadre), 1)
})

