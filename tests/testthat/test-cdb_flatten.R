context("cdb_flatten")

test_that("cdb_flatten works correctly", {
  
  dbf1 <- cdb_flatten(Compadre)
  
  expect_is(dbf1, "data.frame")
  expect_is(dbf1$matA, "character")
  expect_is(dbf1$matU, "character")
  expect_is(dbf1$matF, "character")
  expect_is(dbf1$matC, "character")
  expect_equal(dbf1$SpeciesAuthor, Compadre@data$SpeciesAuthor)
  
  dbf2 <- cdb_flatten(Compadre, onlyMatA = TRUE)
  expect_is(dbf2, "data.frame")
  expect_is(dbf2$matA, "character")
  expect_true(!"matU" %in% names(dbf2))
  expect_true(!"matF" %in% names(dbf2))
  expect_true(!"matC" %in% names(dbf2))
})


test_that("cdb_flatten warns and fails gracefully", {
  
  expect_error(cdb_flatten(Compadre@data))
})

