context("cleanDB")

test_that("cleanDB works correctly", {
  
  db_clean <- cleanDB(Compadre)
  
  expect_s4_class(db_clean, "CompadreDB")
  
  expect_is(db_clean@data$check_NA_A, "logical")
  expect_is(db_clean@data$check_NA_U, "logical")
  expect_is(db_clean@data$check_NA_F, "logical")
  expect_is(db_clean@data$check_NA_C, "logical")
  
  expect_is(db_clean@data$check_ergodic, "logical")
  expect_is(db_clean@data$check_irreducible, "logical")
  expect_is(db_clean@data$check_primitive, "logical")
  expect_is(db_clean@data$check_singular_U, "logical")
})


test_that("cleanDB warns and fails gracefully", {
  
  expect_error(cleanDB(Compadre@data))
})
