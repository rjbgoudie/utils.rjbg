context("stack")

test_that("stack", {
  x <- new_stack(10)
  x$set("one", 1)
  x$set("two", 2)
  x$set("three", 3)
  x$set("four", 4)
  x$set("five", 5)
  x$set("six", 6)
  x$set("seven", 7)
  x$set("eight", 8)
  x$set("nine", 9)
  x$set("ten", 10)

  expected <- c("ten", "nine", "eight", "seven", "six",
                "five", "four", "three", "two", "one")
  expect_that(x$keys_used(),
              is_identical_to(expected))

  x$set("eleven", 11)

  expected <- c("eleven", "ten", "nine", "eight", "seven",
                "six", "five", "four", "three", "two")
  expect_that(x$keys_used(),
              is_identical_to(expected))
})

test_that("stack2", {
  x <- new_stack(10)
  x$set("one", 1)
  x$set("two", 2)
  x$set("three", 3)
  x$set("four", 4)
  x$set("five", 5)
  x$set("six", 6)
  x$set("seven", 7)
  x$set("eight", 8)
  x$set("nine", 9)
  x$set("ten", 10)

  expected <- c("ten", "nine", "eight", "seven", "six",
                "five", "four", "three", "two", "one")
  expect_that(x$keys_used(),
              is_identical_to(expected))

  x$get("one")
  expected <- c("one", "ten", "nine", "eight", "seven",
                "six", "five", "four", "three", "two")
  expect_that(x$keys_used(),
              is_identical_to(expected))

  x$set("eleven", 11)

  expected <- c("eleven", "one", "ten", "nine", "eight",
                "seven", "six", "five", "four", "three")
  expect_that(x$keys_used(),
              is_identical_to(expected))

  expect_that(x$get("one"), is_identical_to(1))
  expect_that(x$get("two"), throws_error())
  expect_that(x$get("three"), is_identical_to(3))
})