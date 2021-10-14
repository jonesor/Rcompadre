test_that("cdb_rbind works correctly", {
  
  db1 <- Compadre[1:50,]
  db2 <- Compadre[51:100,]
  dbm1 <- cdb_rbind(db1, db2)
  
  expect_s4_class(dbm1, "CompadreDB")
  expect_true(nrow(dbm1@data) == nrow(db1@data) + nrow(db2@data))
  expect_true(ncol(dbm1@data) == ncol(db1@data))
  
  db2@version$Version <- "X.Y.Z"
  dbm2 <- cdb_rbind(db1, db2)
  expect_true(is.na(dbm2@version$Version))
})

test_that("cdb_rbind warns and fails gracefully", {
  
  db1 <- Compadre[1:50,]
  db2 <- Compadre[51:100,]
  expect_error(cdb_rbind(db1@data, db2))
  
  db3 <- db2[,-30]
  expect_error(cdb_rbind(db1, db3))
})
