test_that("as_cdb works correctly", {
  db <- as_cdb(CompadreLegacy)

  expect_s4_class(db, "CompadreDB")
  expect_s4_class(db$mat[[1]], "CompadreMat")
  expect_s3_class(db@data, "tbl_df")
  expect_length(db@data$mat, length(CompadreLegacy$mat))
})

test_that("as_cdb warns and fails gracefully", {
  legacy_error1 <- legacy_error2 <- CompadreLegacy
  legacy_error1$mat <- NULL
  legacy_error2$mat[[1]] <- legacy_error2$mat[[1]][2:4]

  expect_error(as_cdb(legacy_error1))
  expect_error(as_cdb(legacy_error2))
})
