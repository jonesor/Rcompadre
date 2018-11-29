context("cdb_fetch")

test_that("cdb_fetch works correctly", {
  
  local_path <- paste0(system.file("testdata", package = "RcompadreDev"),
                       "/CompadreLegacy.RData")
  
  db1 <- cdb_fetch('compadre')  # web
  db2 <- cdb_fetch('comadre')   # web
  db3 <- cdb_fetch(local_path)  # local
  
  expect_s4_class(db1, "CompadreDB")
  expect_s4_class(db2, "CompadreDB")
  expect_s4_class(db3, "CompadreDB")
})

