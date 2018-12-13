context("ClassUnion")

test_that("ClassUnion methods work correctly", {
  
  a1 <- matA(Compadre)
  u1 <- matU(Compadre)
  f1 <- matF(Compadre)
  c1 <- matC(Compadre)
  mc1 <- matrixClass(Compadre)
  mca1 <- MatrixClassAuthor(Compadre)
  mco1 <- MatrixClassOrganized(Compadre)
  mcn1 <- MatrixClassNumber(Compadre)
  
  expect_is(a1, "list")
  expect_is(u1, "list")
  expect_is(f1, "list")
  expect_is(c1, "list")
  expect_is(mc1, "list")
  expect_is(mca1, "list")
  expect_is(mco1, "list")
  expect_is(mcn1, "list")
  
  expect_true(all.equal(length(a1), length(u1), length(f1), length(c1)))
  
  
  a2 <- matA(Compadre@data$mat)
  u2 <- matU(Compadre@data$mat)
  f2 <- matF(Compadre@data$mat)
  c2 <- matC(Compadre@data$mat)
  mc2 <- matrixClass(Compadre@data$mat)
  mca2 <- MatrixClassAuthor(Compadre@data$mat)
  mco2 <- MatrixClassOrganized(Compadre@data$mat)
  mcn2 <- MatrixClassNumber(Compadre@data$mat)
  
  expect_is(a2, "list")
  expect_is(u2, "list")
  expect_is(f2, "list")
  expect_is(c2, "list")
  expect_is(mc2, "list")
  expect_is(mca2, "list")
  expect_is(mco2, "list")
  expect_is(mcn2, "list")
  
  a3 <- matA(Compadre@data$mat[[1]])
  u3 <- matU(Compadre@data$mat[[1]])
  f3 <- matF(Compadre@data$mat[[1]])
  c3 <- matC(Compadre@data$mat[[1]])
  mc3 <- matrixClass(Compadre@data$mat[[1]])
  mca3 <- MatrixClassAuthor(Compadre@data$mat[[1]])
  mco3 <- MatrixClassOrganized(Compadre@data$mat[[1]])
  mcn3 <- MatrixClassNumber(Compadre@data$mat[[1]])
  
  expect_is(a3, "matrix")
  expect_is(u3, "matrix")
  expect_is(f3, "matrix")
  expect_is(c3, "matrix")
  expect_is(mc3, "data.frame")
  expect_is(mca3, "character")
  expect_is(mco3, "character")
  expect_is(mcn3, "numeric")
  
  expect_true(all.equal(a1[[1]], a2[[1]]) & all.equal(a1[[1]], a3))
  expect_true(all.equal(u1[[1]], u2[[1]]) & all.equal(u1[[1]], u3))
  expect_true(all.equal(f1[[1]], f2[[1]]) & all.equal(f1[[1]], f3))
  expect_true(all.equal(c1[[1]], c2[[1]]) & all.equal(c1[[1]], c3))
  expect_true(all.equal(mc1[[1]], mc2[[1]]) & all.equal(mc1[[1]], mc3))
  expect_true(all.equal(mca1[[1]], mca2[[1]]) & all.equal(mca1[[1]], mca3))
  expect_true(all.equal(mco1[[1]], mco2[[1]]) & all.equal(mco1[[1]], mco3))
  expect_true(all.equal(mcn1[[1]], mcn2[[1]]) & all.equal(mcn1[[1]], mcn3))
})


test_that("ClassUnion methods warn and fail gracefully", {
  
  dat <- CompadreData(Compadre)
  dat$mat[[56]] <- "test"
  
  expect_error(matA(dat$mat))
  expect_error(matU(dat$mat))
  expect_error(matF(dat$mat))
  expect_error(matC(dat$mat))
  expect_error(matrixClass(dat$mat))
  expect_error(MatrixClassAuthor(dat$mat))
  expect_error(MatrixClassOrganized(dat$mat))
  expect_error(MatrixClassNumber(dat$mat))
})

