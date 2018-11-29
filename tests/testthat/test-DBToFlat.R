context("DBToFlat")

test_that("DBToFlat works correctly", {
  
  dbf1 <- DBToFlat(Compadre)
  
  expect_is(dbf1, "data.frame")
  expect_is(dbf1$matA, "character")
  expect_is(dbf1$matU, "character")
  expect_is(dbf1$matF, "character")
  expect_is(dbf1$matC, "character")
  expect_equal(dbf1$SpeciesAuthor, Compadre@data$SpeciesAuthor)
  
  dbf2 <- DBToFlat(Compadre, onlyMatA = TRUE)
  expect_is(dbf2, "data.frame")
  expect_is(dbf2$matA, "character")
  expect_true(!"matU" %in% names(dbf2))
  expect_true(!"matF" %in% names(dbf2))
  expect_true(!"matC" %in% names(dbf2))
})


test_that("DBToFlat warns and fails gracefully", {
  
  expect_error(DBToFlat(Compadre@data))
})

