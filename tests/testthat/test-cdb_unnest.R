test_that("cdb_unnest works correctly", {
  d1 <- cdb_unnest(Compadre)

  expect_s4_class(d1, "CompadreDB")
  expect_type(d1$matA, "list")
  expect_type(d1$matU, "list")
  expect_type(d1$matF, "list")
  expect_type(d1$matC, "list")
  expect_type(d1$MatrixClassAuthor, "list")
  expect_type(d1$MatrixClassOrganized, "list")
  expect_type(d1$MatrixClassNumber, "list")
  expect_identical(d1$SpeciesAuthor, Compadre@data$SpeciesAuthor)

  d2 <- cdb_unnest(Compadre, c("matF", "MatrixClassOrganized"))
  expect_s4_class(d2, "CompadreDB")
  expect_false(any(c("matA", "matU", "matC") %in% names(d2)))
})


test_that("cdb_unnest warns and fails gracefully", {
  expect_error(cdb_unnest(Compadre@data))
  expect_error(cdb_unnest(Compadre, "blah"))
})
