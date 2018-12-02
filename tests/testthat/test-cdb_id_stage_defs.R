context("cdb_id_stage_defs")

test_that("cdb_id_stage_defs works correctly", {
  
  id1 <- cdb_id_stage_defs(Compadre, "MatrixClassOrganized")
  expect_is(id1, "integer")
  expect_length(id1, nrow(Compadre@data))
  
  id2 <- cdb_id_stage_defs(Compadre, "MatrixClassAuthor")
  expect_is(id2, "integer")
  expect_length(id2, nrow(Compadre@data))
})


test_that("cdb_id_stage_defs warns and fails gracefully", {
  
  expect_error(cdb_id_stage_defs(Compadre@data, "MatrixClassOrganized"))
  expect_error(cdb_id_stage_defs(Compadre, "blah"))
})

