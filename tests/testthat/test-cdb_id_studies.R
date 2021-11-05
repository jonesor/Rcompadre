test_that("cdb_id_studies works correctly", {
  
  s1 <- cdb_id_studies(Compadre)
  expect_type(s1, "integer")
  expect_length(s1, nrow(Compadre@data))
  
  s2 <- cdb_id_studies(Compadre, c("Family", "MatrixPopulation"))
  expect_type(s2, "integer")
  expect_length(s2, nrow(Compadre@data))
  expect_true(!all(s1 == s2))
})


test_that("cdb_id_studies warns and fails gracefully", {
  
  expect_error(cdb_id_studies(Compadre@data))
  expect_error(cdb_id_studies(Compadre, c("SpeciesAuthor", "blah")))
})

