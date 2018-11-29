context("cdb_check_species")

test_that("cdb_check_species works correctly", {
  
  species <- c("Primula vulgaris", "Trillium ovatum", "Homo sapiens")
  df1 <- cdb_check_species(Compadre, species)
  
  expect_is(df1, "data.frame")
  expect_is(df1$inDatabase, "logical")
  expect_true(nrow(df1) == length(species))
  
  
  db1 <- cdb_check_species(Compadre, species, returnDatabase = TRUE)
  
  expect_s4_class(db1, "CompadreDB")
  expect_true(all(db1$SpeciesAccepted %in% species))
})


test_that("cdb_check_species warns and fails gracefully", {
  
  species <- c("Primula vulgaris", "Trillium ovatum", "Homo sapiens")
  expect_error(cdb_check_species(Compadre@data, species))
})
