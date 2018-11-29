context("compadreDBs")

test_that("compadreDBs works correctly", {
  
  db1 <- Compadre[1:50,]
  db2 <- Compadre[51:100,]
  compareDBs(db1, db2, verbose = TRUE)
  
  expect_output(compareDBs(db1, db2))
  expect_output(compareDBs(db1, db2, verbose = TRUE))
})

test_that("compadreDBs warns and fails gracefully", {
  
  expect_error(compadreDBs(Compadre@data, Compadre))
  expect_error(compadreDBs(Compadre, Compadre@data))
})
