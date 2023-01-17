test_that("cdb_fetch works correctly", {
  local_path <- paste0(
    system.file("testdata", package = "Rcompadre"),
    "/CompadreLegacy.RData"
  )

  # db1 <- cdb_fetch('compadre')  # web
  # db2 <- cdb_fetch('comadre')   # web
  # expect_s4_class(db1, "CompadreDB")
  # expect_s4_class(db2, "CompadreDB")

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
