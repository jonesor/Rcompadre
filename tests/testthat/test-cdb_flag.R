context("cdb_flag")

test_that("cdb_flag works correctly", {
  
  db_clean <- cdb_flag(Compadre)
  
  expect_s4_class(db_clean, "CompadreDB")
  
  expect_is(db_clean@data$check_NA_A, "logical")
  expect_is(db_clean@data$check_NA_U, "logical")
  expect_is(db_clean@data$check_NA_F, "logical")
  expect_is(db_clean@data$check_NA_C, "logical")
  expect_is(db_clean@data$check_zero_U, "logical")
  
  expect_is(db_clean@data$check_ergodic, "logical")
  expect_is(db_clean@data$check_irreducible, "logical")
  expect_is(db_clean@data$check_primitive, "logical")
  expect_is(db_clean@data$check_singular_U, "logical")
})


test_that("cdb_flag warns and fails gracefully", {
  
  expect_error(cdb_flag(Compadre@data))
})
