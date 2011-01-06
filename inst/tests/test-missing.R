context("Remove missingness")

test_that("Basics", {
  dat <- esoph[, 1:3]
  
  expected <- structure(list(agegp = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
  1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 
  3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 
  4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 
  5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L), .Label = c("25-34", 
  "35-44", "45-54", "55-64", "65-74", "75+"), class = c("ordered", 
  "factor")), alcgp = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 
  3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 1L, 1L, 
  1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 
  2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 
  3L, 3L, 1L, 1L, 1L, 2L, 2L, 3L, 3L), .Label = c("0-39g/day", 
  "80-119", "120+"), class = c("ordered", "factor")), tobgp = structure(c(1L, 
  2L, 3L, 4L, 1L, 2L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 
  3L, 4L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 
  4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 
  4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 4L, 1L, 2L, 1L, 2L
  ), .Label = c("0-9g/day", "10-19", "20-29", "30+"), class = c("ordered", 
  "factor"))), .Names = c("agegp", "alcgp", "tobgp"), row.names = c(NA, 
  -65L), class = "data.frame")
  
  expect_that(missingnessRemove(dat, "40-79"),
              is_identical_to(expected))
})

test_that("No missing rows", {
  dat <- esoph[, 1:3]
  
  expect_that(missingnessRemove(dat, "ZZZZMissing"),
              is_identical_to(dat))
})

test_that("Summary", {
  dat <- esoph[, 1:3]
  
  expected <- structure(list(Missing = c(0L, 23L, 0L)),
                            .Names = "Missing",
                            row.names = c("agegp", "alcgp", "tobgp"),
                            class = "data.frame")
  
  expect_that(missingnessSummary(dat, "40-79"),
              is_identical_to(expected))
})