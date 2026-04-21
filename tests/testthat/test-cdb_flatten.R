test_that("cdb_flatten works correctly", {
  dbf1 <- cdb_flatten(Compadre)

  expect_s3_class(dbf1, "data.frame")
  expect_type(dbf1$matA, "character")
  expect_type(dbf1$matU, "character")
  expect_type(dbf1$matF, "character")
  expect_type(dbf1$matC, "character")
  expect_identical(dbf1$SpeciesAuthor, Compadre@data$SpeciesAuthor)

  db_na <- Compadre[1, ]
  db_na@data$mat[[1]]@matC <- matrix(NA, nrow = 2, ncol = 2)
  dbf2 <- cdb_flatten(db_na)
  expect_identical(dbf2$matC[[1]], "[NA NA NA NA]")
})


test_that("cdb_flatten warns and fails gracefully", {
  expect_error(cdb_flatten(Compadre@data))
})
