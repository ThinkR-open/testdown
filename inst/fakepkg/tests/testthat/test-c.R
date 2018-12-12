context("test-c")

test_that("more_than works", {
  expect_gt(2 * 2, 4)
  expect_gt(2 * 3, 4)
  expect_gt(2 * 4, 100)
  expect_gt(nrow(iris), 4)
  expect_gt(nrow(iris), 14)
})

test_that("less_than works", {
  expect_lt(2 * 2, 4)
  expect_lt(2 * 3, 4)
  expect_lt(2 * 4, 100)
  expect_lt(nrow(iris), 4)
  expect_lt(nrow(iris), 14)
})

test_that("true works", {
  expect_true(TRUE)
  expect_true(1 == 1)
  expect_true(1 == 2)
  expect_true(nrow(iris))
})
