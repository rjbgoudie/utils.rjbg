context("factor renamer")

test_that("Basics", {
  dat <- esoph[, 1:3]
  sink(tempfile())
  generateFactorRenamer(dat)
  sink()
  
  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "65-74" = "Old",
    "75+" = "Old"),
  alcgp = c(
    "0-39g/day" = "0-39g/day",
    "40-79" = "40-79",
    "80-119" = "80-119",
    "120+" = "120+"),
  tobgp = c(
    "0-9g/day" = "Light",
    "10-19" = "Medium",
    "20-29" = "Heavy",
    "30+" = "Heavy")
  )
  
  expected <- structure(list(agegp = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
  1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
  1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
  2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
  2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
  3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
  3L), .Label = c("Young", "Middle-aged", "Old"), class = c("ordered", 
  "factor")), alcgp = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
  3L, 3L, 3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 
  3L, 3L, 3L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 
  3L, 3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 
  3L, 3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 
  3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 4L, 4L
  ), .Label = c("0-39g/day", "40-79", "80-119", "120+"), class = c("ordered", 
  "factor")), tobgp = structure(c(1L, 2L, 3L, 3L, 1L, 2L, 3L, 3L, 
  1L, 2L, 3L, 1L, 2L, 3L, 3L, 1L, 2L, 3L, 3L, 1L, 2L, 3L, 3L, 1L, 
  2L, 3L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 3L, 1L, 2L, 3L, 3L, 1L, 2L, 
  3L, 3L, 1L, 2L, 3L, 3L, 1L, 2L, 3L, 3L, 1L, 2L, 3L, 3L, 1L, 2L, 
  3L, 3L, 1L, 2L, 3L, 3L, 1L, 2L, 3L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 
  3L, 1L, 2L, 3L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 3L, 1L, 2L, 1L, 2L
  ), .Label = c("Light", "Medium", "Heavy"), class = c("ordered", 
  "factor"))), .Names = c("agegp", "alcgp", "tobgp"), row.names = c(NA, 
  -88L), class = "data.frame")
  
  expect_that(changeLevels(dat, levelChanges, verbose = F), is_identical_to(expected))
})

test_that("basics", {
  dat <- esoph[, 1:3]
  levelChanges <- list(
  agegp = c(
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "65-74" = "Old",
    "75+" = "Old"),
  alcgp = c(
    "0-39g/day" = "0-39g/day",
    "40-79" = "40-79",
    "80-119" = "80-119",
    "120+" = "120+"),
  tobgp = c(
    "0-9g/day" = "Light",
    "10-19" = "Medium",
    "20-29" = "Heavy",
    "30+" = "Heavy")
  )
  
  processing_output <- 
    paste("Processing: \r  |                               ", 
    "                                             \r  |",
    "=======================                            ",
    "                   |  33%\r  |                     ", 
    "                                                    ",
    "   \r  |===============================================  ", 
    "                     |  67%\r  |                          ",
    "                                                ",
    "  \r  |============================================",
    "==========================| 100%", sep = "")
  
  expect_that(capture.output(changeLevels(dat, levelChanges, verbose = T))[1], 
              is_identical_to(processing_output))
})

test_that("Missing level", {
  dat <- esoph[, 1:3]

  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "75+" = "Old"),
  alcgp = c(
    "0-39g/day" = "0-39g/day",
    "40-79" = "40-79",
    "80-119" = "80-119",
    "120+" = "120+"),
  tobgp = c(
    "0-9g/day" = "Light",
    "10-19" = "Medium",
    "20-29" = "Heavy",
    "30+" = "Heavy")
  )
  
  expect_that(changeLevels(dat, levelChanges, verbose = F), throws_error())
})


test_that("Missing col", {
  dat <- esoph[, 1:3]

  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "65-74" = "Old",
    "75+" = "Old"),
  alcgp = c(
    "0-39g/day" = "0-39g/day",
    "40-79" = "40-79",
    "80-119" = "80-119",
    "120+" = "120+")
  )
  
  expectedError <- "The following columns are missing from levelChanges: tobgp"
  
  expect_that(changeLevels(dat, levelChanges, verbose = F),
              throws_error(expectedError))
})


test_that("Missing cols", {
  dat <- esoph[, 1:3]

  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "65-74" = "Old",
    "75+" = "Old")
  )
  
  
  expect_that(changeLevels(dat, levelChanges, verbose = F),
              throws_error("alcgp, tobgp"))
})

test_that("Missing col, with this allowed", {
  dat <- esoph[, 1:3]

  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "65-74" = "Old",
    "75+" = "Old"),
  alcgp = c(
    "0-39g/day" = "0-39g/day",
    "40-79" = "40-79",
    "80-119" = "80-119",
    "120+" = "120+")
  )
  
  expectedWarning <- "The following columns are missing from levelChanges: tobgp"
  
  expect_that(out <- changeLevels(dat, levelChanges, allowMissingCols = T, verbose = F), 
              gives_warning(expectedWarning))
  
  expect_that(out[, 3],
              is_identical_to(dat[, 3]))
})


test_that("Missing col, with allowed, not not this one", {
  dat <- esoph[, 1:3]

  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "65-74" = "Old",
    "75+" = "Old")
  )
  
  allowMissingCols <- c("alcgp", "tobgp")
  out <- changeLevels(dat, levelChanges, allowMissingCols, verbose = F)
  
  expected <- structure(list(agegp = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
  1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
  1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
  2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
  2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
  3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
  3L), .Label = c("Young", "Middle-aged", "Old"), class = c("ordered", 
  "factor")), alcgp = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
  3L, 3L, 3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 
  3L, 3L, 3L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 
  3L, 3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 
  3L, 3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 
  3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 4L, 4L
  ), .Label = c("0-39g/day", "40-79", "80-119", "120+"), class = c("ordered", 
  "factor")), tobgp = structure(c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 
  1L, 2L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 
  2L, 3L, 4L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 
  3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 
  3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 1L, 2L, 3L, 
  4L, 1L, 2L, 3L, 4L, 1L, 2L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 1L, 2L
  ), .Label = c("0-9g/day", "10-19", "20-29", "30+"), class = c("ordered", 
  "factor"))), .Names = c("agegp", "alcgp", "tobgp"), row.names = c(NA, 
  -88L), class = "data.frame")
  
  expect_that(out, 
              is_identical_to(expected))
})



test_that("Missing col, with allowed, not not this one", {
  dat <- esoph[, 1:3]

  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "65-74" = "Old",
    "75+" = "Old")
  )
  
  allowMissingCols <- c("alcgp")
  expect_that(changeLevels(dat, levelChanges, allowMissingCols, verbose = F), 
              throws_error())
})


test_that("Inconsistent level", {
  dat <- esoph[, 1:3]
  
  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "xxxxx" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "65-74" = "Old",
    "75+" = "Old"),
  alcgp = c(
    "0-39g/day" = "0-39g/day",
    "40-79" = "40-79",
    "80-119" = "80-119",
    "120+" = "120+"),
  tobgp = c(
    "0-9g/day" = "Light",
    "10-19" = "Medium",
    "20-29" = "Heavy",
    "30+" = "Heavy")
  )
  error <- paste("There is a mismatch between the existing levels and the", 
                 "levelChanges of the following columns: agegp", sep = " ")
  
  expect_that(is.consistent.changeLevels(dat, levelChanges, allowMissingCols),
              throws_error(error))
})

test_that("Reordering", {
  dat <- esoph[, 1:3]
  
  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "65-74" = "Old",
    "75+" = "Old",
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged"),
  alcgp = c(
    "0-39g/day" = "0-39g/day",
    "40-79" = "40-79",
    "80-119" = "80-119",
    "120+" = "120+"),
  tobgp = c(
    "0-9g/day" = "Light",
    "10-19" = "Medium",
    "20-29" = "Heavy",
    "30+" = "Heavy")
  )
  
  # we want to reverse the order of agegp too.
  expected <- c("Old", "Young", "Middle-aged")
  
  expect_that(levels(changeLevels(dat, levelChanges, verbose = F)[, 1]),
              is_identical_to(expected))
})


test_that("Multiple incorrect levels, should report all", {
  dat <- esoph[, 1:3]
  
  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "65-74" = "Old",
    "75+" = "Old",
    "xxxxxxxxx" = "xxxxxxx"),
  alcgp = c(
    "0-39g/day" = "0-39g/day",
    "40-79" = "40-79",
    "80-119" = "80-119",
    "120+" = "120+"),
  tobgp = c(
    "0-9g/day" = "Light",
    "10-19" = "Medium",
    "20-29" = "Heavy",
    "30+" = "Heavy",
    "xxxxxxxxx" = "xxxxxxx")
  )
  
  expect_that(changeLevels(dat, levelChanges),
              throws_error("the following columns: agegp, tobgp"))
})


test_that("Mixture of unneeded and missing", {
  dat <- esoph[, 1:3]
  
  # we can then change this into the following
  levelChanges <- list(
  agegp = c(
    "25-34" = "Young",
    "35-44" = "Young",
    "45-54" = "Middle-aged",
    "55-64" = "Middle-aged",
    "65-74" = "Old"),
  alcgp = c(
    "0-39g/day" = "0-39g/day",
    "40-79" = "40-79",
    "80-119" = "80-119",
    "120+" = "120+",
    "xxxxx" = "xxxxxx"),
  tobgp = c(
    "0-9g/day" = "Light",
    "10-19" = "Medium",
    "30+" = "Heavy",
    "xxxxxxxxx" = "xxxxxxx")
  )
  
  expect_that(changeLevels(dat, levelChanges),
              throws_error(paste("Missing from proposed: 20-29", 
                                 "Unneeded in proposed: xxxxxxxxx",
                                 sep = "\n")))
})

