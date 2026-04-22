test_that("CompadreDB-Methods work correctly", {
  df <- as.data.frame(Compadre)
  expect_s3_class(df, "data.frame")

  tb <- tibble::as_tibble(Compadre)
  expect_s3_class(tb, "tbl_df")

  hd <- head(Compadre, n = 10)
  expect_s4_class(hd, "CompadreDB")
  expect_identical(nrow(hd@data), 10L)

  tl <- tail(Compadre, n = 10)
  expect_s4_class(tl, "CompadreDB")
  expect_identical(nrow(tl@data), 10L)

  nm <- names(Compadre)
  expect_identical(nm, names(Compadre@data))

  cnm <- colnames(Compadre)
  expect_identical(cnm, colnames(Compadre@data))

  dm <- dim(Compadre)
  expect_true(inherits(dm, "integer"))
  expect_length(dm, 2)

  expect_output(print(Compadre, n = 20), "20")

  db_drop <- subset(Compadre, Family == "Poaceae" | Family == "Cistaceae")
  expect_gt(nlevels(db_drop@data$Family), 2)
  db_drop <- droplevels(db_drop)
  expect_s4_class(db_drop, "CompadreDB")
  expect_identical(nlevels(db_drop@data$Family), 2L)
  expect_true(all(levels(db_drop@data$Family) %in% c("Cistaceae", "Poaceae")))

  n_row <- nrow(Compadre)
  n_col <- ncol(Compadre)
  expect_type(n_row, "integer")
  expect_type(n_col, "integer")

  traits <- data.frame(
    SpeciesAccepted = sample(Compadre$SpeciesAccepted, 5),
    value = 1:5
  )

  CompadreMerge <- merge(Compadre, traits)
  expect_s4_class(CompadreMerge, "CompadreDB")
  expect_true(all(CompadreMerge$value %in% traits$value))

  db_rbind <- rbind(Compadre[1:2, ], Compadre[3:4, ])
  expect_s4_class(db_rbind, "CompadreDB")
  expect_identical(nrow(db_rbind@data), 4L)

  db_cbind <- cbind(Compadre[1:3, ], extra = 1:3)
  expect_s4_class(db_cbind, "CompadreDB")
  expect_identical(db_cbind@data$extra, 1:3)

  db_cbind2 <- cbind(
    Compadre[1:3, ],
    data.frame(extra2 = c("a", "b", "c"), stringsAsFactors = FALSE)
  )
  expect_s4_class(db_cbind2, "CompadreDB")
  expect_identical(db_cbind2@data$extra2, c("a", "b", "c"))

  nspecies <- NumberAcceptedSpecies(Compadre)
  nstudies <- NumberStudies(Compadre)
  nmatrices <- NumberMatrices(Compadre)

  expect_type(nspecies, "integer")
  expect_type(nstudies, "integer")
  expect_type(nmatrices, "integer")
})


test_that("Number_ functions fail gracefully", {
  comp_nospp <- Compadre[, -which(names(Compadre) == "SpeciesAccepted")]
  comp_noauth <- Compadre[, -which(names(Compadre) == "Authors")]

  expect_error(NumberAcceptedSpecies(comp_nospp))
  expect_error(NumberStudies(comp_noauth))
})


test_that("cbind and rbind fail gracefully", {
  expect_error(cbind(Compadre[1:2, ], Compadre[3:4, ]))
  expect_error(cbind(Compadre[1:2, ], extra = 1:3))
  expect_error(cbind(Compadre[1:2, ], data.frame(extra = 1)))
})
