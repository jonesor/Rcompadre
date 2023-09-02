suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))

test_that("CompadreDB-Tidyverse functions work correctly", {
  # ggplot fortify
  cf <- fortify(Compadre)
  expect_s3_class(cf, "data.frame")

  p <- ggplot(cf, aes(Lon, Lat)) +
    geom_point()

  expect_true(inherits(p, "ggplot"))

  # filter
  db1 <- filter(Compadre, MatrixDimension == 3, Family == "Compositae")
  expect_s4_class(db1, "CompadreDB")
  expect_true(all(db1$MatrixDimension == 3))
  expect_true(all(db1$Family == "Compositae"))

  # slice
  db2 <- slice(Compadre, 11:15)
  expect_s4_class(db2, "CompadreDB")
  expect_identical(nrow(db2@data), 5L)

  # arrange
  db3 <- arrange(Compadre, SurvivalIssue)
  expect_s4_class(db3, "CompadreDB")
  x <- db3$SurvivalIssue[!is.na(db3$SurvivalIssue)]
  expect_true(all(x == cummax(x))) # test sorted

  # mutate
  db4 <- mutate(Compadre, newcol = 5L)
  expect_s4_class(db4, "CompadreDB")
  expect_true(all(db4$newcol, 5L))

  # group_by
  db5 <- group_by(Compadre, Family)
  expect_s4_class(db5, "CompadreDB")
  expect_true(inherits(db5@data, "grouped_df"))

  # summarize
  sum1 <- summarize(db5, n = n())
  sum2 <- summarise(db5, n = n())
  expect_true(inherits(sum1, "tbl"))
  expect_true(inherits(sum2, "tbl"))

  # ungroup
  db6 <- ungroup(db5)
  expect_s4_class(db6, "CompadreDB")
  expect_false(inherits(db6@data, "grouped_df"))

  # select
  db7 <- select(Compadre, mat, SpeciesAccepted, -MatrixDimension)
  expect_s4_class(db7, "CompadreDB")
  expect_true(all(names(db7) == c("mat", "SpeciesAccepted")))

  # rename
  db8 <- rename(Compadre, Species = SpeciesAuthor, doi = DOI_ISBN)
  expect_s4_class(db8, "CompadreDB")
  expect_true("Species" %in% names(db8@data))
  expect_true("doi" %in% names(db8@data))

  # joins
  traits <- data.frame(
    SpeciesAccepted = sample(unique(Compadre$SpeciesAccepted), 5),
    trait = 1:5,
    stringsAsFactors = FALSE
  )

  db9 <- left_join(Compadre, traits, by = "SpeciesAccepted")
  expect_s4_class(db9, "CompadreDB")
  expect_identical(nrow(db9@data), nrow(Compadre@data))
  expect_true(all(traits$trait %in% db9$trait))

  db10 <- right_join(Compadre, traits, by = "SpeciesAccepted")
  expect_s4_class(db10, "CompadreDB")
  expect_true(all(db10$SpeciesAccepted %in% traits$SpeciesAccepted))
  expect_true(all(db10$trait %in% traits$trait))

  db11 <- inner_join(Compadre, traits, by = "SpeciesAccepted")
  expect_s4_class(db11, "CompadreDB")
  expect_true(all(db11$SpeciesAccepted %in% traits$SpeciesAccepted))
  expect_true(all(db11$trait %in% traits$trait))

  db12 <- full_join(Compadre, traits, by = "SpeciesAccepted")
  expect_s4_class(db12, "CompadreDB")
  expect_identical(nrow(db12@data), nrow(Compadre@data))
  expect_true(all(traits$trait %in% db12$trait))

  traits_dup <- rbind(traits, traits[5, ])
  expect_warning(left_join(Compadre, traits_dup, by = "SpeciesAccepted"))
  expect_warning(right_join(Compadre, traits_dup, by = "SpeciesAccepted"))
  expect_warning(inner_join(Compadre, traits_dup, by = "SpeciesAccepted"))
  expect_warning(full_join(Compadre, traits_dup, by = "SpeciesAccepted"))
})
