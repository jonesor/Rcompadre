#mat_median and mpm_median
set.seed(12)

# create a function that generates a matrix with random values
create_matrix <- function() {
  matrix(runif(9, 0, 1), nrow = 3)
}

# use replicate() to call the create_matrix() function 20 times
mat_list <- replicate(20, create_matrix(), simplify = FALSE)

# print the list of matrices
mat_list

testthat::expect_true(inherits(mat_median(mat_list), "matrix"))

# create a function that generates a matrix with random values
create_matrix_with_NA <- function() {
  m1 <- matrix(runif(9, 0, 1), nrow = 3)
  m1[sample(3, 1), sample(3, 1)] <- NA
  return(m1)
}

# use replicate() to call the create_matrix() function 20 times
mat_list_NA <- replicate(20, create_matrix_with_NA(), simplify = FALSE)

# print the list of matrices
mat_list_NA

testthat::expect_true(inherits(mat_median(mat_list_NA, na.rm = TRUE), "matrix"))
testthat::expect_true(inherits(mat_median(mat_list_NA, na.rm = FALSE), "matrix"))

# create a function that generates a matrix with different dimensions
create_matrix_r_dim <- function() {
  m_dim <- sample(2:10, 1)
  matrix(runif(m_dim^2, 0, 1), nrow = m_dim)
}

# use replicate() to call the create_matrix() function 20 times
mat_list_r_dim <- replicate(20, create_matrix_r_dim(), simplify = FALSE)

# print the list of matrices
mat_list_r_dim

testthat::expect_error(mat_median(mat_list_r_dim))

