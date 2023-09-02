test_that("mpm_mean works correctly", {
  mpms1 <- Compadre$mat[Compadre$SpeciesAuthor == "Haplopappus_radiatus"]
  x1 <- mpm_mean(mpms1)
  expect_s4_class(x1, "CompadreMat")
  expect_length(x1, 1)

  mats1 <- list(matrix(0:3, nrow = 2))
  y1 <- mat_mean(mats1)
  expect_true(inherits(y1, "matrix"))

  expect_true(all(y1 == mats1[[1]]))

  mats2 <- list(matrix(0:3, nrow = 2), matrix(1:4, nrow = 2))
  y2 <- mat_mean(mats2)
  expect_true(inherits(y2, "matrix"))
  expect_identical(dim(y2), c(2L, 2L))

  # na handling
  mpms1[[1]]@matA[1, 1] <- NA
  z1 <- mpm_mean(mpms1)
  expect_true(is.na(z1@matA[1, 1]))

  z2 <- mpm_mean(mpms1, na.rm = TRUE)
  expect_false(is.na(z2@matA[1, 1]))

  mats_na <- list(matrix(0:3, nrow = 2), matrix(1:4, nrow = 2))
  mats_na[[1]][1, 1] <- NA

  z3 <- mat_mean(mats_na)
  expect_true(is.na(z3[1, 1]))

  z4 <- mat_mean(mats_na, na.rm = TRUE)
  expect_false(is.na(z4[1, 1]))

  mats_na[[2]][1, 1] <- NA
  z5 <- mat_mean(mats_na, na.rm = TRUE)
  expect_identical(z5[1, 1], 0)
})

test_that("mpm_mean warns and fails gracefully", {
  mats3 <- list(matrix(0:3, nrow = 2), matrix(1:9, nrow = 3))
  expect_error(mat_mean(mats3))
})
