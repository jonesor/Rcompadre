context("cdb_id_stages")

test_that("cdb_id_stages works correctly", {
  
  id1 <- cdb_id_stages(Compadre, "MatrixClassOrganized")
  expect_type(id1, "integer")
  expect_length(id1, nrow(Compadre@data))
  
  id2 <- cdb_id_stages(Compadre, "MatrixClassAuthor")
  expect_type(id2, "integer")
  expect_length(id2, nrow(Compadre@data))
})


test_that("cdb_id_stages warns and fails gracefully", {
  
  expect_error(cdb_id_stages(Compadre@data, "MatrixClassOrganized"))
  expect_error(cdb_id_stages(Compadre, "blah"))
})

