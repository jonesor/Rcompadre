test_that("CompadreDB-Subsetting works correctly", {

  # [
  sub1 <- Compadre[1:5, ]
  expect_s4_class(sub1, "CompadreDB")
  expect_true(nrow(sub1@data) == 5)
  expect_true(ncol(sub1@data) == ncol(Compadre@data))

  sub2 <- Compadre[1:5, 1:5]
  expect_true(nrow(sub2@data) == 5)
  expect_true(ncol(sub2@data) == 5)

  sub3 <- Compadre[, 1:10]
  expect_true(ncol(sub3@data) == 10)

  sub4 <- Compadre[, -10]
  expect_true(ncol(sub4@data) == ncol(Compadre@data) - 1)

  sub5 <- suppressWarnings(Compadre[, -c(1:2)])
  expect_true(ncol(sub5@data) == ncol(Compadre@data) - 1)

  sub6 <- Compadre[, c("mat", "SpeciesAuthor")]
  expect_true(ncol(sub6@data) == 2)

  sub7 <- Compadre[, names(Compadre) %in% c("mat", "SpeciesAuthor")]
  expect_true(ncol(sub7@data) == 2)

  # subset()
  sub1 <- subset(Compadre, SpeciesAccepted == "Lechea cernua")
  n1 <- length(which(Compadre@data$SpeciesAccepted == "Lechea cernua"))
  expect_s4_class(sub1, "CompadreDB")
  expect_true(nrow(sub1@data) == n1)

  sub2 <- subset(Compadre, select = c("mat", "SpeciesAccepted"))
  expect_true(ncol(sub2@data) == 2)

  sub3 <- subset(Compadre, MatrixDimension == 4, select = 1:5)
  n3 <- length(which(Compadre@data$MatrixDimension == 4))
  expect_true(nrow(sub3@data) == n3)
  expect_true(ncol(sub3@data) == 5)
})


test_that("CompadreDB-Subsetting warns and fails gracefully", {

  # [
  expect_warning(Compadre[, -1])
  expect_warning(Compadre[, "SpeciesAccepted"])
  expect_warning(Compadre[, names(Compadre) != "mat"])
  expect_error(Compadre[, expression(-1)])

  # subset()
  expect_error(subset(Compadre, 1:5))
  expect_warning(subset(Compadre, select = 5:10))
})
