context("cdb_subset_wild")

test_that("cdb_subset_wild works correctly", {
  
  sub1 <- cdb_subset_wild(Compadre)
  expect_s4_class(sub1, "CompadreDB")
  expect_true(all(sub1@data$MatrixCaptivity == "W"))
  expect_true(all(sub1@data$MatrixTreatment == "Unmanipulated"))
  
  sub2 <- cdb_subset_wild(Comadre)
  expect_s4_class(sub2, "CompadreDB")
  expect_true(all(sub2@data$MatrixCaptivity == "W"))
  expect_true(all(sub2@data$MatrixTreatment == "Unmanipulated"))
})


test_that("cdb_subset_wild warns and fails gracefully", {
  
  expect_error(cdb_subset_wild(Compadre@data))
})

