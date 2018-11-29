context("asCompadreDB")

test_that("asCompadreDB works correctly", {
  
  db <- asCompadreDB(CompadreLegacy)
  
  expect_s4_class(db, "CompadreDB")
  expect_s4_class(db$mat[[1]], "CompadreMat")
  expect_is(db@data, "tbl_df")
  expect_length(db@data$mat, length(CompadreLegacy$mat))
})

test_that("asCompadreDB warns and fails gracefully", {
  
  legacy_error1 <- legacy_error2 <- CompadreLegacy
  legacy_error1$mat <- NULL
  legacy_error2$mat[[1]] <- legacy_error2$mat[[1]][2:4]
  
  expect_error(asCompadreDB(legacy_error1))
  expect_error(asCompadreDB(legacy_error2))
})
