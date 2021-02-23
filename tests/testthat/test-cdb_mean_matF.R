context("cdb_mean_matF")

test_that("cdb_mean_matF works correctly", {
  
  x <- cdb_mean_matF(Compadre)
  
  expect_is(x, "list")
  expect_is(x[[1]], "matrix")
  expect_length(x, nrow(Compadre@data))
})

test_that("cdb_mean_matF warns and fails gracefully", {
  
  expect_error(cdb_mean_matF(Compadre@data))
})
