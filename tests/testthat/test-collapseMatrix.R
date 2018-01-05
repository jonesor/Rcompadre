context('Test collapseMatrix')

collapse1 <- c("1-2", "3-4", "5")
collapse2 <- c("1-2", "3-4-5")
collapse3 <- c("1-2-3-4-5")

matU <- matrix(c(0.1, 0.0, 0.0, 0.0, 0.0, 
                 0.2, 0.2, 0.1, 0.0, 0.0, 
                 0.1, 0.4, 0.2, 0.2, 0.0, 
                 0.0, 0.1, 0.5, 0.6, 0.2, 
                 0.0, 0.1, 0.5, 0.1, 0.7), nrow = 5, byrow = TRUE)
matF <- matrix(c(0.0, 0.0, 0.0, 5.4, 8.3, 
                 0.0, 0.0, 0.0, 0.0, 0.0, 
                 0.0, 0.0, 0.0, 0.0, 0.0, 
                 0.0, 0.0, 0.0, 0.0, 0.0, 
                 0.0, 0.0, 0.0, 0.0, 0.0), nrow = 5, byrow = TRUE)
matC <- matrix(rep(0, 25), nrow = 5, byrow = TRUE)

is_square <- function(x) {
  return(dim(x)[1] == dim(x)[2])
}

test_that('stages are correctly collapsed', {
  # testing to 6 digits per matrix element. we can go higher if
  # we find that there are problems with this cut off
  c1 <- collapseMatrix(matU, matF, matC, collapse1)
  c2 <- collapseMatrix(matU, matF, matC, collapse2)
  # matA
  expect_equal(as.vector(t(c1$matA)), c(0.28652295, 2.4556453,  8.3,
                                        0.15390821, 0.7444461,  0.2,
                                        0.01347705, 0.3222155,  0.7),
               tolerance = 1e-6)
  expect_equal(as.vector(t(c2$matA)), c(0.2865229, 4.253956,
                                        0.1673853, 1.01538),
               tolerance = 1e-6)
  
  # matU
  expect_equal(as.vector(t(c1$matU)), c(0.28652295, 0.05555386, 0,
                                        0.15390821, 0.74444614,  0.2,
                                        0.01347705, 0.32221545,  0.7),
               tolerance = 1e-6)
  expect_equal(as.vector(t(c2$matU)), c(0.28652295, 0.03845992,
                                        0.16738526, 1.015379575),
               tolerance = 1e-6)
  
  # matF
  expect_equal(as.vector(t(c1$matF)), c(0, 2.400091, 8.3,
                                        0, 0, 0,
                                        0, 0, 0),
               tolerance = 1e-6)
  expect_equal(as.vector(t(c2$matF)), c(0, 4.215496,
                                        0, 0),
               tolerance = 1e-6)
})

test_that('returned matrices are always square', {
  c1 <- collapseMatrix(matU, matF, matC, collapse1)
  c2 <- collapseMatrix(matU, matF, matC, collapse2)
  expect_true(all(unlist(lapply(c1, FUN = is_square))))
  expect_true(all(unlist(lapply(c2, FUN = is_square))))
})
