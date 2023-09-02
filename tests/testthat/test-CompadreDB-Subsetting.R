test_that("CompadreDB-Subsetting works correctly", {
  # [
  sub1 <- Compadre[1:5, ]
  expect_s4_class(sub1, "CompadreDB")
  expect_identical(nrow(sub1@data), 5L)
  expect_identical(ncol(sub1@data), ncol(Compadre@data))

  sub2 <- Compadre[1:5, 1:5]
  expect_identical(nrow(sub2@data), 5L)
  expect_identical(ncol(sub2@data), 5L)

  sub3 <- Compadre[, 1:10]
  expect_identical(ncol(sub3@data), 10L)

  sub4 <- Compadre[, -10]
  expect_identical(ncol(sub4@data), as.integer(ncol(Compadre@data) - 1))

  sub5 <- suppressWarnings(Compadre[, -(1:2)])
  expect_identical(ncol(sub5@data), as.integer(ncol(Compadre@data) - 1))

  sub6 <- Compadre[, c("mat", "SpeciesAuthor")]
  expect_identical(ncol(sub6@data), 2L)

  sub7 <- Compadre[, names(Compadre) %in% c("mat", "SpeciesAuthor")]
  expect_identical(ncol(sub7@data), 2L)

  sub1 <- subset(Compadre, SpeciesAccepted == "Lechea cernua")
  n1 <- length(which(Compadre@data$SpeciesAccepted == "Lechea cernua"))
  expect_s4_class(sub1, "CompadreDB")
  expect_identical(nrow(sub1@data), n1)

  sub2 <- subset(Compadre, select = c("mat", "SpeciesAccepted"))
  expect_identical(ncol(sub2@data), 2L)

  sub3 <- subset(Compadre, MatrixDimension == 4, select = 1:5)
  n3 <- length(which(Compadre@data$MatrixDimension == 4))
  expect_identical(nrow(sub3@data), n3)
  expect_identical(ncol(sub3@data), 5L)
})


test_that("CompadreDB-Subsetting warns and fails gracefully", {
  expect_warning(Compadre[, -1])
  expect_warning(Compadre[, "SpeciesAccepted"])
  expect_warning(Compadre[, names(Compadre) != "mat"])
  expect_error(Compadre[, expression(-1)])

  expect_error(subset(Compadre, 1:5))
  expect_warning(subset(Compadre, select = 5:10))
})
