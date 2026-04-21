test_that("cdb_export_matlab writes Matlab-friendly flat files", {
  csv_file <- tempfile(fileext = ".csv")
  tsv_file <- tempfile(fileext = ".tsv")

  csv_path <- cdb_export_matlab(Compadre[1:3, ], csv_file)
  tsv_path <- cdb_export_matlab(Compadre[1:3, ], tsv_file, format = "tsv")

  expect_true(file.exists(csv_file))
  expect_true(file.exists(tsv_file))
  expect_identical(csv_path, normalizePath(csv_file, winslash = "/", mustWork = FALSE))
  expect_identical(tsv_path, normalizePath(tsv_file, winslash = "/", mustWork = FALSE))

  csv_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
  tsv_data <- read.delim(tsv_file, stringsAsFactors = FALSE, check.names = FALSE)
  flat_data <- cdb_flatten(Compadre[1:3, ])

  expect_identical(names(csv_data), names(flat_data))
  expect_identical(names(tsv_data), names(flat_data))
  expect_identical(csv_data$matA[[1]], flat_data$matA[[1]])
  expect_identical(tsv_data$matC[[1]], flat_data$matC[[1]])
  expect_identical(csv_data$SpeciesAuthor[[2]], as.character(flat_data$SpeciesAuthor[[2]]))
})

test_that("cdb_export_matlab fails gracefully", {
  out_file <- tempfile(fileext = ".csv")

  expect_error(cdb_export_matlab(Compadre@data, out_file))
  expect_error(cdb_export_matlab(Compadre, ""))
  expect_error(cdb_export_matlab(Compadre, out_file, format = "bad"))
})
