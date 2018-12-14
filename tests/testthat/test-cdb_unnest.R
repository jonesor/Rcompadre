context("cdb_unnest")

test_that("cdb_unnest works correctly", {
  
  d1 <- cdb_unnest(Compadre)
  
  expect_s4_class(d1, "CompadreDB")
  expect_is(d1$matA, "list")
  expect_is(d1$matU, "list")
  expect_is(d1$matF, "list")
  expect_is(d1$matC, "list")
  expect_is(d1$MatrixClassAuthor, "list")
  expect_is(d1$MatrixClassOrganized, "list")
  expect_is(d1$MatrixClassNumber, "list")
  expect_equal(d1$SpeciesAuthor, Compadre@data$SpeciesAuthor)
  
  d2 <- cdb_unnest(Compadre, c("matF", "MatrixClassOrganized"))
  expect_s4_class(d2, "CompadreDB")
  expect_true(!any(c("matA", "matU", "matC") %in% names(d2)))
})


test_that("cdb_unnest warns and fails gracefully", {
  
  expect_error(cdb_unnest(Compadre@data))
  expect_error(cdb_unnest(Compadre, "blah"))
})

