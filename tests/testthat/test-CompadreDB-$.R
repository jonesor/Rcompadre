context("CompadreDB $ and $<-")

test_that("CompadreDB $ and $<- methods work correctly", {
  
  expect_equal(Compadre$SpeciesAuthor, Compadre@data$SpeciesAuthor)
  
  db1 <- Compadre
  db1$ones <- 1L
  expect_true(ncol(db1@data) == ncol(Compadre@data) + 1)
  expect_true(all(db1$ones == 1L))
})


test_that("CompadreDB $ and $<- methods warn and fail gracefully", {
  
  expect_error(Compadre$mat <- 1L)
})
