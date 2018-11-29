context("string_to_mat")

test_that("string_to_mat works correctly", {
  
  s1 <- "[3.3 5.2 6.1 0.1 0.2 0.3 0.2 0.4 0.1]"
  m1 <- string_to_mat(s1)
  
  expect_is(m1, "matrix")
  expect_is(c(m1), "numeric")
  expect_true(!any(is.na(m1)))
  
  s2 <- "[3.3 5.2 6.1 0.1 NA 0.3 0.2 0.4 0.1]"
  m2 <- string_to_mat(s2)
  expect_true(any(is.na(m2)))
})

test_that("string_to_mat warns and fails gracefully", {
  
  s3 <- "[3.3 5.2 6.1 0.1 NA 0.3 0.2 0.4 0.1 0.4]"
  expect_error(string_to_mat(s3))
})

