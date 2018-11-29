context("ClassUnion")

test_that("ClassUnion methods work correctly", {
  
  a1 <- matA(Compadre)
  u1 <- matU(Compadre)
  f1 <- matF(Compadre)
  c1 <- matC(Compadre)
  mc1 <- matrixClass(Compadre)
  mca1 <- MatrixClassAuthor(Compadre)
  mco1 <- MatrixClassOrganized(Compadre)
  
  expect_is(a1, "list")
  expect_is(u1, "list")
  expect_is(f1, "list")
  expect_is(c1, "list")
  expect_is(mc1, "list")
  expect_is(mca1, "list")
  expect_is(mco1, "list")
  
  expect_true(all.equal(length(a1), length(u1), length(f1), length(c1)))
  
  a2 <- matA(Compadre@data$mat[[1]])
  u2 <- matU(Compadre@data$mat[[1]])
  f2 <- matF(Compadre@data$mat[[1]])
  c2 <- matC(Compadre@data$mat[[1]])
  mc2 <- matrixClass(Compadre@data$mat[[1]])
  mca2 <- MatrixClassAuthor(Compadre@data$mat[[1]])
  mco2 <- MatrixClassOrganized(Compadre@data$mat[[1]])
  
  expect_is(a2, "matrix")
  expect_is(u2, "matrix")
  expect_is(f2, "matrix")
  expect_is(c2, "matrix")
  expect_is(mc2, "data.frame")
  expect_is(mca2, "character")
  expect_is(mco2, "character")
  
  expect_equal(a1[[1]], a2)
  expect_equal(u1[[1]], u2)
  expect_equal(f1[[1]], f2)
  expect_equal(c1[[1]], c2)
  expect_equal(mc1[[1]], mc2)
  expect_equal(mca1[[1]], mca2)
  expect_equal(mco1[[1]], mco2)
})

