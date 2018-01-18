context('identifyReproStages')

# matrix with no ambiguous columns
mat1 <- matrix(c(NA, NA, 1.2,
                 0, 0, 0,
                 0, 0, 0),
               nrow = 3, byrow = TRUE)

# matrix with an column that has both NA and a number
mat2 <- matrix(c(NA, NA, 1.2,
                 2, 0, 0,
                 0, 0, 0),
               nrow = 3, byrow = TRUE)

# all NAs
matNA <- matrix(rep(NA, 9),
                nrow = 3)

# all 0s
mat0 <- matrix(rep(0, 9),
               nrow = 3)

test_that('fails correctly', {
  skip_on_cran()
  expect_error(identifyReproStages(mat1, na.handling = 'NA'),
               regexp = "Argument na.handling must be either 'return.na', 'return.true', or 'return.false'")
  expect_error(identifyReproStages(matNA, na.handling = 'NA'),
               regexp = "Argument na.handling must be either 'return.na', 'return.true', or 'return.false'")
  
  expect_error(identifyReproStages(matNA, na.handling = 'return.na'),
               regexp = "All elements of matF are NA")
})

test_that('returns correct values for each matrix', {
  skip_on_cran()
  
  # Matrix1
  expect_identical(identifyReproStages(mat1, na.handling = 'return.na'),
                   c(NA, NA, TRUE))
  expect_identical(identifyReproStages(mat1, na.handling = 'return.true'),
                   rep(TRUE, 3))
  expect_identical(identifyReproStages(mat1, na.handling = 'return.false'),
                   c(FALSE, FALSE, TRUE))
  
  # Matrix 2
  expect_identical(identifyReproStages(mat2, na.handling = 'return.na'),
                   c( TRUE, NA, TRUE))
  expect_identical(identifyReproStages(mat2, na.handling = 'return.true'),
                   rep(TRUE, 3))
  expect_identical(identifyReproStages(mat2, na.handling = 'return.false'),
                   c(TRUE, FALSE, TRUE))
  
  # Matrix of 0s
  expect_identical(identifyReproStages(mat0, na.handling = 'return.na'),
                   rep(FALSE, 3))
  expect_identical(identifyReproStages(mat0, na.handling = 'return.true'),
                   rep(FALSE, 3))
  expect_identical(identifyReproStages(mat0, na.handling = 'return.false'),
                   rep(FALSE, 3))
  
})
