context("fetchDB")

test_that("fetchDB works correctly", {
  
  local_path <- paste0(system.file("testdata", package = "RcompadreDev"),
                       "/CompadreLegacy.RData")
  
  db1 <- fetchDB('compadre')  # web
  db2 <- fetchDB('comadre')   # web
  db3 <- fetchDB(local_path)  # local
  
  expect_s4_class(db1, "CompadreDB")
  expect_s4_class(db2, "CompadreDB")
  expect_s4_class(db3, "CompadreDB")
})

