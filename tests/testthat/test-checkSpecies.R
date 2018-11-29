context("checkSpecies")

test_that("checkSpecies works correctly", {
  
  species <- c("Primula vulgaris", "Trillium ovatum", "Homo sapiens")
  df1 <- checkSpecies(species, Compadre)
  
  expect_is(df1, "data.frame")
  expect_is(df1$inDatabase, "logical")
  expect_true(nrow(df1) == length(species))
  
  
  db1 <- checkSpecies(species, Compadre, returnDatabase = TRUE)
  
  expect_s4_class(db1, "CompadreDB")
  expect_true(all(db1$SpeciesAccepted %in% species))
})


test_that("checkSpecies warns and fails gracefully", {
  
  species <- c("Primula vulgaris", "Trillium ovatum", "Homo sapiens")
  expect_error(checkSpecies(species, Compadre@data))
})
