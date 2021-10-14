test_that("cdb_id works correctly", {
  
  id1 <- cdb_id(Compadre, c("SpeciesAuthor", "MatrixPopulation"))
  expect_type(id1, "integer")
  expect_length(id1, nrow(Compadre@data))
  
  id2 <- cdb_id(Compadre, "Family")
  expect_type(id2, "integer")
  expect_length(id2, nrow(Compadre@data))
  expect_true(length(unique(id2)) == length(unique(Compadre@data$Family)))
})


test_that("cdb_id warns and fails gracefully", {
  
  expect_error(cdb_id(Compadre@data))
  expect_error(cdb_id(Compadre, c("SpeciesAuthor", "blah")))
})

