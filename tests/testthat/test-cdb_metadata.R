test_that("cdb_metadata works correctly", {
  Compadre_metadata <- cdb_metadata(Compadre)

  expect_s3_class(Compadre_metadata, "tbl_df")
  expect_identical(ncol(Compadre_metadata), as.integer(ncol(Compadre) - 1))
})


test_that("cdb_metadata warns and fails gracefully", {
  Compadre_metadata <- cdb_metadata(Compadre)

  expect_error(cdb_metadata(Compadre_metadata))
})
