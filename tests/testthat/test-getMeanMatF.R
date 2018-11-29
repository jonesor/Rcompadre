context("getMeanMatF")

test_that("getMeanMatF works correctly", {
  
  x <- getMeanMatF(Compadre)
  
  expect_is(x, "list")
  expect_is(x[[1]], "matrix")
  expect_length(x, nrow(Compadre@data))
})

test_that("getMeanMatF warns and fails gracefully", {
  
  expect_error(getMeanMatF(Compadre@data))
})
