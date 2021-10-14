test_that("cdb_unflatten works correctly", {
  
  db1 <- cdb_flatten(Compadre)
  CompadreUnflat <- cdb_unflatten(db1)
  
  expect_s4_class(CompadreUnflat, "CompadreDB")
  expect_equal(CompadreUnflat$SpeciesAuthor, Compadre$SpeciesAuthor)
  expect_true(all(matA(CompadreUnflat)[[65]] == matA(Compadre)[[65]]))
})


test_that("cdb_unflatten warns and fails gracefully", {
  
  expect_error(cdb_unflatten(Compadre))
})

