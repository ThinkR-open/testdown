context("test-a")

test_that("equal works", {
  expect_equal(2 * 2, 4)
  expect_equal(2 * 2, 5)
  expect_equal(2 * 4, 6)
  expect_equal(2 * 7, 14)
  expect_equal(2 * 7, 4)
})

test_that("length works", {
  expect_length(1:10, 10)
  expect_length(1:100, 10)
  expect_length(1:1000, 1000)
  expect_length(1:5, 5)
  expect_length(1:5, 100)
})

test_that("is works", {
  expect_is(1:10, "integer")
  expect_is(1:10, "character")
  expect_is(1:10, "data.frame")
  expect_is(iris, "numeric")
  expect_is(iris, "integer")
  expect_is(iris, "data.frame")
})

