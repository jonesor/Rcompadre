context("cdb_flatten")

test_that("cdb_flatten works correctly", {
  
  dbf1 <- cdb_flatten(Compadre)
  
  expect_is(dbf1, "data.frame")
  expect_is(dbf1$matA, "character")
  expect_is(dbf1$matU, "character")
  expect_is(dbf1$matF, "character")
  expect_is(dbf1$matC, "character")
  expect_equal(dbf1$SpeciesAuthor, Compadre@data$SpeciesAuthor)
})


test_that("cdb_flatten warns and fails gracefully", {
  
  expect_error(cdb_flatten(Compadre@data))
})

