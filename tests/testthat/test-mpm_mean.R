context("mpm_mean")

test_that("mpm_mean works correctly", {
  
  mpms1 <- Compadre$mat[Compadre$SpeciesAuthor == "Haplopappus_radiatus"]
  x1 <- mpm_mean(mpms1)
  expect_s4_class(x1, "CompadreMat")
  expect_length(x1, 1)
  
  mats1 <- list(matrix(0:3, nrow = 2))
  y1 <- mat_mean(mats1)
  expect_is(y1, "matrix")
  expect_true(all(y1 == mats1[[1]]))
  
  mats2 <- list(matrix(0:3, nrow = 2), matrix(1:4, nrow = 2))
  y2 <- mat_mean(mats2)
  expect_is(y2, "matrix")
  expect_true(all(dim(y2) == c(2, 2)))
})

test_that("mpm_mean warns and fails gracefully", {
  
  mats3 <- list(matrix(0:3, nrow = 2), matrix(1:9, nrow = 3))
  expect_error(mat_mean(mats3))
  
  mpms2 <- Compadre$mat[Compadre$MatrixDimension == 3]
  expect_warning(mpm_mean(mpms2))
})

