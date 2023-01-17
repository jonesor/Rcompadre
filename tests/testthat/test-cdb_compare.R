test_that("cdb_compare works correctly", {
  db1 <- Compadre[1:50, ]
  db2 <- Compadre[51:100, ]

  expect_output(cdb_compare(db1, db2))
  expect_output(cdb_compare(db1, db2, verbose = TRUE))
})

test_that("cdb_compare warns and fails gracefully", {
  expect_error(cdb_compare(Compadre@data, Compadre))
  expect_error(cdb_compare(Compadre, Compadre@data))
})
