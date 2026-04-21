test_that("CompadreDB works correctly", {
  db1 <- db2 <- db3 <- db4 <- as_cdb(CompadreLegacy)

  expect_true(isTRUE(methods::validObject(db1, test = TRUE)))
  expect_output(print(db1))

  # missing mat column
  db2@data <- db2@data[, 5:10]
  expect_false(isTRUE(methods::validObject(db2, test = TRUE)))

  # mat not a list-column
  db3@data$mat <- 1L
  expect_false(isTRUE(methods::validObject(db3, test = TRUE)))

  # mat a list-column but without CompadreMat objects
  db4@data$mat <- CompadreLegacy$mat
  expect_false(isTRUE(methods::validObject(db4, test = TRUE)))

  # $ and $<-
  expect_identical(Compadre$SpeciesAuthor, Compadre@data$SpeciesAuthor)

  db1 <- Compadre
  db1$ones <- 1L
  expect_identical(ncol(db1@data), as.integer((ncol(Compadre@data) + 1)))
  expect_true(all(db1$ones == 1L))

  expect_error(Compadre$mat <- 1L)

  # [[ and [[<-
  expect_identical(Compadre[["SpeciesAuthor"]], Compadre@data$SpeciesAuthor)

  db1 <- Compadre
  db1[["ones"]] <- 1L
  expect_identical(ncol(db1@data), as.integer(ncol(Compadre@data) + 1))
  expect_true(all(db1$ones == 1L))

  expect_error(Compadre[["mat"]] <- 1L)

  # accessors
  expect_s3_class(CompadreData(Compadre), "data.frame")
  expect_type(VersionData(Compadre), "list")
  expect_length(Version(Compadre), 1)
  expect_length(DateCreated(Compadre), 1)
  expect_length(DateCreated(Compadre), 1)
})
