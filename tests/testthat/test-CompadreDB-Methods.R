context("CompadreDB-Methods")

test_that("CompadreDB-Methods work correctly", {
  
  df <- as.data.frame(Compadre)
  expect_s3_class(df, "data.frame")
  
  tb <- as_tibble(Compadre)
  expect_s3_class(tb, "tbl_df")
  
  hd <- head(Compadre, n = 10)
  expect_s4_class(hd, "CompadreDB")
  expect_true(nrow(hd@data) == 10)
  
  tl <- tail(Compadre, n = 10)
  expect_s4_class(tl, "CompadreDB")
  expect_true(nrow(tl@data) == 10)
  
  nm <- names(Compadre)
  expect_equal(nm, names(Compadre@data))
  
  dm <- dim(Compadre)
  expect_true("integer" == class(dm))
  expect_length(dm, 2)
  
  n_row <- nrow(Compadre)
  n_col <- ncol(Compadre)
  expect_type(n_row, "integer")
  expect_type(n_col, "integer")
  
  traits <- data.frame(SpeciesAccepted = sample(Compadre$SpeciesAccepted, 5),
                       value = 1:5)
  
  CompadreMerge <- merge(Compadre, traits)
  expect_s4_class(CompadreMerge, "CompadreDB")
  expect_true(all(CompadreMerge$value %in% traits$value))
  
  nspecies <- NumberAcceptedSpecies(Compadre)
  nstudies <- NumberStudies(Compadre)
  nmatrices <- NumberMatrices(Compadre)
  
  expect_type(nspecies, "integer")
  expect_type(nstudies, "integer")
  expect_type(nmatrices, "integer")
})



test_that("Number_ functions fail gracefully", {
  
  comp_nospp <- Compadre[,-which(names(Compadre) == "SpeciesAccepted")]
  comp_noauth <- Compadre[,-which(names(Compadre) == "Authors")]
  
  expect_error(NumberAcceptedSpecies(comp_nospp))
  expect_error(NumberStudies(comp_noauth))
})



