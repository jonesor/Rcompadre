test_that("cdb_mean_matF works correctly", {
  
  x <- cdb_mean_matF(Compadre)
  
  expect_type(x, "list")
  expect_true("matrix" %in% class(x[[1]]))
  expect_length(x, nrow(Compadre@data))
})

test_that("cdb_mean_matF warns and fails gracefully", {
  
  expect_error(cdb_mean_matF(Compadre@data))
})
