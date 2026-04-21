test_that("cdb_rbind works correctly", {
  db1 <- Compadre[1:50, ]
  db2 <- Compadre[51:100, ]
  db3 <- Compadre[101:150, ]
  dbm1 <- cdb_rbind(db1, db2)
  dbm_multi <- cdb_rbind(db1, db2, db3)

  expect_s4_class(dbm1, "CompadreDB")
  expect_identical(nrow(dbm1@data), nrow(db1@data) + nrow(db2@data))
  expect_identical(ncol(dbm1@data), ncol(db1@data))
  expect_s4_class(dbm_multi, "CompadreDB")
  expect_identical(
    nrow(dbm_multi@data),
    nrow(db1@data) + nrow(db2@data) + nrow(db3@data)
  )
  expect_identical(ncol(dbm_multi@data), ncol(db1@data))

  db2@version$Version <- "X.Y.Z"
  dbm2 <- cdb_rbind(db1, db2)
  expect_true(is.na(dbm2@version$Version))

  db3@version$Version <- "Y.Z.A"
  dbm3 <- cdb_rbind(db1, Compadre[51:100, ], db3)
  expect_true(is.na(dbm3@version$Version))

  db4 <- Compadre[1:3, ]
  db5 <- Compadre[4:6, ]
  db4@data$extra_chr <- c("a", "b", "c")
  db5@data$extra_num <- c(1, 2, 3)
  dbm_fill <- cdb_rbind(db4, db5, fill = TRUE)

  expect_s4_class(dbm_fill, "CompadreDB")
  expect_identical(nrow(dbm_fill@data), 6L)
  expect_true(all(c("extra_chr", "extra_num") %in% names(dbm_fill@data)))
  expect_identical(dbm_fill@data$extra_chr[1:3], c("a", "b", "c"))
  expect_true(all(is.na(dbm_fill@data$extra_chr[4:6])))
  expect_identical(dbm_fill@data$extra_num[4:6], c(1, 2, 3))
  expect_true(all(is.na(dbm_fill@data$extra_num[1:3])))

  db6 <- Compadre[1:2, ]
  db7 <- Compadre[3:4, ]
  db6@data$extra_factor <- factor(c("x", "y"))
  dbm_factor <- cdb_rbind(db6, db7, fill = TRUE)
  expect_true("extra_factor" %in% names(dbm_factor@data))
  expect_true(is.factor(dbm_factor@data$extra_factor))
  expect_identical(levels(dbm_factor@data$extra_factor), c("x", "y"))
  expect_true(all(is.na(dbm_factor@data$extra_factor[3:4])))
})

test_that("cdb_rbind warns and fails gracefully", {
  db1 <- Compadre[1:50, ]
  db2 <- Compadre[51:100, ]
  expect_error(cdb_rbind(db1@data, db2))
  expect_error(cdb_rbind(db1))

  db3 <- db2[, -30]
  expect_error(cdb_rbind(db1, db3))
  expect_s4_class(cdb_rbind(db1, db3, fill = TRUE), "CompadreDB")
})
