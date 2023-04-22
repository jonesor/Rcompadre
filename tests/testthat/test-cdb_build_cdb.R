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

my_compadre <- cdb_build_cdb(mat_a = mat_a_list, version = "testrun")

testthat::expect_true(inherits(my_compadre, "CompadreDB"))

testthat::expect_error(my_compadre <- cdb_build_cdb( version = "testrun"))

testthat::expect_error(cdb_build_cdb(mat_a = mat_a_list, 
                                     mat_u = mat_a_list, 
                                     version = "testrun"))

testthat::expect_error(cdb_build_cdb(mat_u = mat_a_list, 
                                     version = "testrun"))

testthat::expect_error(cdb_build_cdb(mat_c = mat_a_list, 
                                     version = "testrun"))

my_compadre_2<-cdb_build_cdb(mat_u = mat_a_list,
                                     mat_f = mat_a_list,
                                     version = "testrun")
testthat::expect_true(inherits(my_compadre_2, "CompadreDB"))
