context("cdb_tidy")

test_that("cdb_tidy works correctly", {
  
  dbf1 <- cdb_tidy(Compadre)
  
  expect_is(dbf1, "data.frame")
  expect_is(dbf1$matA, "list")
  expect_is(dbf1$matU, "list")
  expect_is(dbf1$matF, "list")
  expect_is(dbf1$matC, "list")
  expect_is(dbf1$MatrixClassAuthor, "list")
  expect_is(dbf1$MatrixClassOrganized, "list")
  expect_equal(dbf1$SpeciesAuthor, Compadre@data$SpeciesAuthor)
})


test_that("cdb_tidy warns and fails gracefully", {
  
  expect_error(cdb_tidy(Compadre@data))
})

