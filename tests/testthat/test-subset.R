context("subset")

test_that("subset method for CompadreDB works correctly", {
  
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


test_that("subset method for CompadreDB warns and fails gracefully", {
  
  expect_error(subset(Compadre, 1:5))
  expect_warning(subset(Compadre, select = 5:10))
})
