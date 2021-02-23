context("string_representation")

test_that("string_representation functions work correctly", {
  
  ms1 <- "[3.3 5.2 6.1 0.1 0.2 0.3 0.2 0.4 0.1]"
  m1 <- string_to_mat(ms1)
  
  expect_true("matrix" %in% class(m1))
  expect_true("numeric" %in% class(c(m1)))
  
  expect_true(!any(is.na(m1)))
  expect_true(nrow(m1) == ncol(m1))
  
  ms2 <- "[3.3 5.2 6.1 0.1 NA 0.3 0.2 0.4 0.1]"
  m2 <- string_to_mat(ms2)
  expect_true(any(is.na(m2)))
  
  vs1 <- "[0.2||0.5||0.3]"
  v1 <- string_to_vec(vs1, numeric = TRUE)
  expect_true("numeric" %in% class(v1))
  
  expect_length(v1, 3)
  
  vs2 <- "[Seed Bank||Reproductive Small (0.6-1.4 cm)||Dormant]"
  v2 <- string_to_vec(vs2)
  expect_type(v2, "character")
  expect_length(v2, 3)
  
  vs3 <- "[1.0]"
  v3 <- string_to_vec(vs3, numeric = TRUE)
  expect_true("numeric" %in% class(v3))
  expect_length(v3, 1)
  
  ms1_reconv <- mat_to_string(m1)
  expect_type(ms1_reconv, "character")
  expect_length(ms1_reconv, 1)
  expect_match(ms1_reconv, "^\\[")
  expect_match(ms1_reconv, "\\]$")
  
  ms2_reconv <- mat_to_string(m2)
  expect_type(ms2_reconv, "character")
  expect_length(ms2_reconv, 1)
  expect_match(ms2_reconv, " NA ")
  
  vs1_reconv <- vec_to_string(v1)
  expect_type(vs1_reconv, "character")
  expect_length(vs1_reconv, 1)
  expect_match(vs1_reconv, "^\\[")
  expect_match(vs1_reconv, "\\]$")
  expect_match(vs1_reconv, "\\|\\|")
  
  vs2_reconv <- vec_to_string(v2)
  expect_type(vs2_reconv, "character")
  expect_length(vs2_reconv, 1)
  expect_match(vs2_reconv, "^\\[")
  expect_match(vs2_reconv, "\\]$")
  expect_match(vs2_reconv, "\\|\\|")
})

test_that("string_representation functions warn and fail gracefully", {
  
  s1 <- "[3.3 5.2 6.1]"
  expect_error(string_to_mat(s1))
  
  m1 <- matrix(1:6, nrow = 2)
  expect_error(mat_to_string(m1))
})

