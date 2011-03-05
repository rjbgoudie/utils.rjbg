# context("gcinfo")
# 
# test_that("parse_gcinfo", {
#   input <- c("Garbage collection 253 = 116+63+74 (level 2) ... ",
#              "20.1 Mbytes of cons cells used (56%)",
#              "3.8 Mbytes of vectors used (22%)")
#   
#   expect_that(parse_gcinfo(input),
#               is_identical_to(c(cons = 20.1, vecs = 3.8)))
# })