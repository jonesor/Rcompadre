context("CompadreDB-Tidyverse")

suppressMessages(library(ggplot2))

test_that("CompadreDB-Tidyverse functions work correctly", {
  
  cf <- fortify(Compadre)
  expect_is(cf, "data.frame")
  
  p <- ggplot(cf, aes(Lon, Lat)) + geom_point()
  expect_is(p, "ggplot")
})

