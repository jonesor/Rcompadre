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

  expect_true(inherits(cdb_fetch(local_path, flag = TRUE), "CompadreDB"))
  expect_true(inherits(
    cdb_fetch(local_path, userComment = "a comment goes here"),
    "CompadreDB"
  ))
})

#test_that("cdb_fetch works correctly using internet", {
#  # Check obtaining via internet
#  expect_true(inherits(cdb_fetch("compadre"), "CompadreDB"))
#  expect_true(inherits(cdb_fetch("comadre"), "CompadreDB"))
#  expect_true(inherits(cdb_fetch("compadre", version = "5.0.1"), "CompadreDB"))
#  expect_true(inherits(cdb_fetch("comadre", version = "4.23.3.1"), "CompadreDB"))
#
#
#x <- cdb_fetch("compadre")
#testthat::expect_true(inherits(x, "CompadreDB"))
#
#x <- cdb_fetch("compadre", flag = FALSE)
#testthat::expect_true(inherits(x, "CompadreDB"))
#})