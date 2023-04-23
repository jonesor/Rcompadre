test_that("cdb_fetch works correctly", {
  local_path <- paste0(
    system.file("testdata", package = "Rcompadre"),
    "/CompadreLegacy.RData"
  )

  db3 <- cdb_fetch(local_path) # local
  expect_s4_class(db3, "CompadreDB")

  # fetch .RData already in CompadreDB format
  temp <- tempdir()
  file_path <- paste0(temp, "/comp_test.RData")
  save(db3, file = file_path)
  db4 <- cdb_fetch(file_path)
  expect_s4_class(db4, "CompadreDB")

  unlink(temp)
})
