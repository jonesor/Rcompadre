test_that("CompadreMat works correctly", {
  cmat1 <- cmat2 <- cmat3 <- cmat4 <- methods::new(
    "CompadreMat",
    matA = CompadreLegacy$mat[[5]]$matA,
    matU = CompadreLegacy$mat[[5]]$matU,
    matF = CompadreLegacy$mat[[5]]$matF,
    matC = CompadreLegacy$mat[[5]]$matC,
    matrixClass = as.data.frame(CompadreLegacy$matrixClass[[5]])
  )

  expect_s4_class(cmat1, "CompadreMat")
  expect_true(isTRUE(methods::validObject(cmat1, test = TRUE)))
  expect_output(print(cmat1))

  # missing MatrixClassAuthor
  cmat2@matrixClass$MatrixClassAuthor <- NULL
  expect_false(isTRUE(methods::validObject(cmat2, test = TRUE)))

  # non-square matrix
  cmat3@matU <- matrix(1:6, nrow = 2)
  expect_false(isTRUE(methods::validObject(cmat3, test = TRUE)))

  cmat4@matrixClass <- rbind(cmat4@matrixClass, cmat4@matrixClass)
  expect_false(isTRUE(methods::validObject(cmat4, test = TRUE)))
})
