context("CompadreDB-Methods")

test_that("CompadreDB-Methods work correctly", {
  
  df <- as.data.frame(Compadre)
  expect_is(df, "data.frame")
  
  tb <- as_tibble(Compadre)
  expect_is(tb, "tbl_df")
  
  hd <- head(Compadre, n = 10)
  expect_s4_class(hd, "CompadreDB")
  expect_true(nrow(hd@data) == 10)
  
  tl <- tail(Compadre, n = 10)
  expect_s4_class(tl, "CompadreDB")
  expect_true(nrow(tl@data) == 10)
  
  nm <- names(Compadre)
  expect_equal(nm, names(Compadre@data))
  
  traits <- data.frame(SpeciesAccepted = sample(Compadre$SpeciesAccepted, 5),
                       value = 1:5)
  
  CompadreMerge <- merge(Compadre, traits)
  expect_s4_class(CompadreMerge, "CompadreDB")
  expect_true(all(CompadreMerge$value %in% traits$value))
  
  nspecies <- NumberAcceptedSpecies(Compadre)
  nstudies <- NumberStudies(Compadre)
  nmatrices <- NumberMatrices(Compadre)
  
  expect_is(nspecies, "integer")
  expect_is(nstudies, "integer")
  expect_is(nmatrices, "integer")
})

