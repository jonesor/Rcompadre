test_that("cdb_collapse works correctly", {
  CompSub <- subset(Compadre, MatrixComposite != "Seasonal")
  CompSub$IdStage <- cdb_id_stages(CompSub, "MatrixClassAuthor")
  CompCollapse <- cdb_collapse(CompSub, columns = "IdStage")

  expect_s4_class(CompCollapse, "CompadreDB")
  expect_identical(nrow(CompCollapse@data), length(unique(CompSub$IdStage)))
})


test_that("cdb_collapse warns and fails gracefully", {
  # contains Seasonal
  Compadre$IdStage <- cdb_id_stages(Compadre, "MatrixClassAuthor")
  expect_warning(cdb_collapse(Compadre, "IdStage"))

  # not CompadreDB
  CompSub <- subset(Compadre, MatrixComposite != "Seasonal")
  CompSub$IdStage <- cdb_id_stages(CompSub, "MatrixClassAuthor")
  expect_error(cdb_collapse(CompSub@data, "IdStage"))
})
