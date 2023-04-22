suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))


# ggplot fortify
cf <- fortify(Compadre)
testthat::expect_s3_class(cf, "data.frame")

p <- ggplot(cf, aes(Lon, Lat)) +
  geom_point()
testthat::expect_true("ggplot" %in% class(p))

# filter
db1 <- filter(Compadre, MatrixDimension == 3, Family == "Compositae")
testthat::expect_s4_class(db1, "CompadreDB")
testthat::expect_true(all(db1$MatrixDimension == 3))
testthat::expect_true(all(db1$Family == "Compositae"))

# slice
db2 <- slice(Compadre, 11:15)
testthat::expect_s4_class(db2, "CompadreDB")
testthat::expect_true(nrow(db2@data) == 5)

# arrange
db3 <- arrange(Compadre, SurvivalIssue)
testthat::expect_s4_class(db3, "CompadreDB")
x <- db3$SurvivalIssue[!is.na(db3$SurvivalIssue)]
testthat::expect_true(all(x == cummax(x))) # test sorted

# mutate
db4 <- mutate(Compadre, newcol = 5L)
testthat::expect_s4_class(db4, "CompadreDB")
testthat::expect_true(all(db4$newcol == 5L))

# group_by
db5 <- group_by(Compadre, Family)
testthat::expect_s4_class(db5, "CompadreDB")
testthat::expect_true("grouped_df" %in% class(db5@data))

# summarize
sum1 <- summarize(db5, n = n())
sum2 <- summarise(db5, n = n())
testthat::expect_true("tbl" %in% class(sum1))
testthat::expect_true("tbl" %in% class(sum2))

# ungroup
db6 <- ungroup(db5)
testthat::expect_s4_class(db6, "CompadreDB")
testthat::expect_false("grouped_df" %in% class(db6@data))

# select
db7 <- select(Compadre, mat, SpeciesAccepted, -MatrixDimension)
testthat::expect_s4_class(db7, "CompadreDB")
testthat::expect_true(all(names(db7) == c("mat", "SpeciesAccepted")))

# rename
db8 <- rename(Compadre, Species = SpeciesAuthor, doi = DOI_ISBN)
testthat::expect_s4_class(db8, "CompadreDB")
testthat::expect_true("Species" %in% names(db8@data))
testthat::expect_true("doi" %in% names(db8@data))

# joins
traits <- data.frame(
  SpeciesAccepted = sample(unique(Compadre$SpeciesAccepted), 5),
  trait = 1:5,
  stringsAsFactors = FALSE
)

db9 <- left_join(Compadre, traits, by = "SpeciesAccepted")
testthat::expect_s4_class(db9, "CompadreDB")
testthat::expect_true(nrow(db9@data) == nrow(Compadre@data))
testthat::expect_true(all(traits$trait %in% db9$trait))

db10 <- right_join(Compadre, traits, by = "SpeciesAccepted")
testthat::expect_s4_class(db10, "CompadreDB")
testthat::expect_true(all(db10$SpeciesAccepted %in% traits$SpeciesAccepted))
testthat::expect_true(all(db10$trait %in% traits$trait))

db11 <- inner_join(Compadre, traits, by = "SpeciesAccepted")
testthat::expect_s4_class(db11, "CompadreDB")
testthat::expect_true(all(db11$SpeciesAccepted %in% traits$SpeciesAccepted))
testthat::expect_true(all(db11$trait %in% traits$trait))

db12 <- full_join(Compadre, traits, by = "SpeciesAccepted")
testthat::expect_s4_class(db12, "CompadreDB")
testthat::expect_true(nrow(db12@data) == nrow(Compadre@data))
testthat::expect_true(all(traits$trait %in% db12$trait))

