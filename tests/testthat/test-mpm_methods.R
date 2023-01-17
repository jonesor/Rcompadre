test_that("mpm_methods methods work correctly", {
  prop_db <- mpm_has_prop(Compadre)
  active_db <- mpm_has_active(Compadre)
  dorm_db <- mpm_has_dorm(Compadre)
  first_active_db <- mpm_first_active(Compadre)

  expect_type(prop_db, "logical")
  expect_type(active_db, "logical")
  expect_type(dorm_db, "logical")
  expect_type(first_active_db, "integer")

  prop_mat <- mpm_has_prop(Compadre@data$mat[[15]])
  active_mat <- mpm_has_active(Compadre@data$mat[[15]])
  dorm_mat <- mpm_has_dorm(Compadre@data$mat[[15]])
  first_active_mat <- mpm_first_active(Compadre@data$mat[[15]])

  expect_type(prop_mat, "logical")
  expect_type(active_mat, "logical")
  expect_type(dorm_mat, "logical")
  expect_type(first_active_mat, "integer")
})
