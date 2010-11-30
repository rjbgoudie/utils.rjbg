context("notdiag()")

test_that("Basics", {
  m <- matrix(c(0,1,2,3), 2, 2)
  expect_identical(notdiag(m), c(1, 2))
})