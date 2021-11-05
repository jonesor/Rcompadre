test_that("cdb_flag works correctly", {
  
  db_clean <- cdb_flag(Compadre)
  
  expect_s4_class(db_clean, "CompadreDB")
  
  expect_type(db_clean@data$check_NA_A, "logical")
  expect_type(db_clean@data$check_NA_U, "logical")
  expect_type(db_clean@data$check_NA_F, "logical")
  expect_type(db_clean@data$check_NA_C, "logical")
  expect_type(db_clean@data$check_zero_U, "logical")
  
  expect_type(db_clean@data$check_singular_U, "logical")
  expect_type(db_clean@data$check_component_sum, "logical")
  expect_type(db_clean@data$check_ergodic, "logical")
  expect_type(db_clean@data$check_irreducible, "logical")
  expect_type(db_clean@data$check_primitive, "logical")
  
  db_clean2 <- cdb_flag(Compadre, checks = c("check_ergodic",
                                             "check_irreducible",
                                             "check_primitive"))
  
  expect_s4_class(db_clean2, "CompadreDB")
  expect_true("check_ergodic" %in% names(db_clean2))
  expect_true("check_irreducible" %in% names(db_clean2))
  expect_true("check_primitive" %in% names(db_clean2))
  expect_true(!"check_NA_A" %in% names(db_clean2))
  expect_true(!"check_NA_U" %in% names(db_clean2))
  expect_true(!"check_NA_F" %in% names(db_clean2))
  expect_true(!"check_NA_C" %in% names(db_clean2))
  expect_true(!"check_zero_U" %in% names(db_clean2))
  expect_true(!"check_singular_U" %in% names(db_clean2))
  expect_true(!"check_component_sum" %in% names(db_clean2))
})


test_that("cdb_flag warns and fails gracefully", {
  
  expect_error(cdb_flag(Compadre@data))
  expect_error(cdb_flag(Compadre, checks = "blah"))
})
