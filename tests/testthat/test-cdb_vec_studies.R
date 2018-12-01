context("cdb_vec_studies")

test_that("cdb_vec_studies works correctly", {
  
  s1 <- cdb_vec_studies(Compadre)
  expect_is(s1, "integer")
  expect_length(s1, nrow(Compadre@data))
  
  s2 <- cdb_vec_studies(Compadre, c("Family", "MatrixPopulation"))
  expect_is(s2, "integer")
  expect_length(s2, nrow(Compadre@data))
  expect_true(!all(s1 == s2))
})


test_that("cdb_vec_studies warns and fails gracefully", {
  
  expect_error(cdb_vec_studies(Compadre@data))
  expect_error(cdb_vec_studies(Compadre, c("SpeciesAuthor", "blah")))
})

