test_that("test that cdb_build_cdb functions properly", {
  # If you only have A matrices
  mat_a1 <- rbind(
    c(0.1, 1.9),
    c(0.5, 0.7)
  )
  mat_a2 <- rbind(
    c(0.2, 1.4, 2.3),
    c(0.6, 0.3, 1.1),
    c(0.2, 0.2, 1.5)
  )
  mat_a3 <- rbind(
    c(0.1, 2.1),
    c(0.3, 0.4)
  )
  # Place the matrices into a list
  mat_a_list <- mget(ls(pattern = "mat_a[0-9]"))

  testthat::expect_true(inherits(
    cdb_build_cdb(
      mat_a = mat_a_list,
      version = "testrun"
    ),
    "CompadreDB"
  ))
})



test_that("test that cdb_build_cdb functions with various matrices", {
  # add U and F matrices
  mat_u1 <- rbind(
    c(0.1, 0.0),
    c(0.5, 0.7)
  )
  mat_u2 <- rbind(
    c(0.2, 0.0, 0.0),
    c(0.6, 0.3, 1.1),
    c(0.2, 0.2, 1.5)
  )
  mat_f1 <- rbind(
    c(0.0, 1.9),
    c(0.0, 0.0)
  )
  mat_f2 <- rbind(
    c(0.0, 1.4, 2.3),
    c(0.0, 0.0, 0.0),
    c(0.0, 0.0, 0.0)
  )
  mat_c1 <- rbind(
    c(0.0, 0.2),
    c(0.0, 0.0)
  )
  mat_c2 <- rbind(
    c(0.0, 0.4, 0.3),
    c(0.0, 0.0, 0.0),
    c(0.0, 0.0, 0.0)
  )
  mat_u_list <- mget(ls(pattern = "mat_u[0-9]"))
  mat_f_list <- mget(ls(pattern = "mat_f[0-9]"))
  mat_c_list <- mget(ls(pattern = "mat_c[0-9]"))

  meta <- data.frame(idNum = 1:2, SpeciesAccepted = c("A", "B"), x = 4:5)
  stageInfo <- list(
    data.frame(
      MatrixClassOrganized = rep("active", 2),
      MatrixClassAuthor = c("small", "large")
    ),
    data.frame(
      MatrixClassOrganized = rep("active", 3),
      MatrixClassAuthor = c("small", "medium", "large")
    )
  )

  stageInfo_with_error1 <- list(
    data.frame(
      MatrixClassOrganized = rep("active", 2),
      MatrixClassBadName = c("small", "large")
    ),
    data.frame(
      MatrixClassOrganized = rep("active", 3),
      MatrixClassBadName = c("small", "medium", "large")
    )
  )

  stageInfo_with_error2 <- list(
    data.frame(
      MatrixClassBadName = rep("active", 2),
      MatrixClassAuthor = c("small", "large")
    ),
    data.frame(
      MatrixClassBadName = rep("active", 3),
      MatrixClassAuthor = c("small", "medium", "large")
    )
  )

  testthat::expect_error(cdb_build_cdb(
    mat_u = NULL, mat_f = NULL,
    metadata = meta, stages = stageInfo
  ))

  testthat::expect_error(cdb_build_cdb(
    mat_u = mat_u_list,
    metadata = meta, stages = stageInfo
  ))

  testthat::expect_error(cdb_build_cdb(
    mat_a = mat_a_list,
    mat_u = mat_u_list,
    mat_f = mat_f_list,
    metadata = meta, stages = stageInfo
  ))

  testthat::expect_error(cdb_build_cdb(
    mat_c = mat_c_list,
    metadata = meta, stages = stageInfo
  ))
  testthat::expect_error(cdb_build_cdb(
    metadata = meta, stages = stageInfo
  ))



  testthat::expect_true(inherits(cdb_build_cdb(
    mat_u = mat_u_list, mat_f = mat_f_list,
    metadata = meta, stages = stageInfo
  ), "CompadreDB"))

  testthat::expect_true(inherits(cdb_build_cdb(
    mat_u = mat_u_list, mat_f = mat_f_list, mat_c = mat_c_list,
    metadata = meta, stages = stageInfo
  ), "CompadreDB"))

  testthat::expect_error(cdb_build_cdb(
    mat_u = mat_u_list, mat_f = mat_f_list,
    metadata = rbind(meta, meta), stages = stageInfo
  ))


  testthat::expect_error(cdb_build_cdb(
    mat_u = mat_u_list, mat_f = mat_f_list,
    metadata = meta, stages = stageInfo[[2]]
  ))

  testthat::expect_true(inherits(cdb_build_cdb(
    mat_u = mat_u_list, mat_f = mat_f_list,
    metadata = meta
  ), "CompadreDB"))

  testthat::expect_error(cdb_build_cdb(
    mat_u = mat_u_list, mat_f = mat_f_list,
    metadata = meta, stages = stageInfo_with_error1
  ))
})

# No matrices


# Bad dimensions

test_that("test that cdb_build_cdb functions with bad dimension matrices", {
  # add U and F matrices
  mat_u2 <- rbind(
    c(0.1, 0.0),
    c(0.5, 0.7)
  )
  mat_u1 <- rbind(
    c(0.2, 0.0, 0.0),
    c(0.6, 0.3, 1.1),
    c(0.2, 0.2, 1.5)
  )
  mat_f1 <- rbind(
    c(0.0, 1.9),
    c(0.0, 0.0)
  )
  mat_f2 <- rbind(
    c(0.0, 1.4, 2.3),
    c(0.0, 0.0, 0.0),
    c(0.0, 0.0, 0.0)
  )
  mat_u_list <- mget(ls(pattern = "mat_u[0-9]"))
  mat_f_list <- mget(ls(pattern = "mat_f[0-9]"))
  meta <- data.frame(idNum = 1:2, SpeciesAccepted = c("A", "B"), x = 4:5)
  stageInfo <- list(
    data.frame(
      MatrixClassOrganized = rep("active", 2),
      MatrixClassAuthor = c("small", "large")
    ),
    data.frame(
      MatrixClassOrganized = rep("active", 3),
      MatrixClassAuthor = c("small", "medium", "large")
    )
  )

  testthat::expect_error(cdb_build_cdb(
    mat_u = mat_u_list, mat_f = mat_f_list,
    metadata = meta, stages = stageInfo
  ))
})
