test_that("cdb_unflatten works correctly", {
  db1 <- cdb_flatten(Compadre)
  CompadreUnflat <- cdb_unflatten(db1)

  expect_s4_class(CompadreUnflat, "CompadreDB")
  expect_identical(CompadreUnflat$SpeciesAuthor, Compadre$SpeciesAuthor)
  expect_identical(
    as.vector(matA(CompadreUnflat)[[65]]),
    as.vector(matA(Compadre)[[65]])
  )
})


test_that("cdb_unflatten warns and fails gracefully", {
  expect_error(cdb_unflatten(Compadre))
})
