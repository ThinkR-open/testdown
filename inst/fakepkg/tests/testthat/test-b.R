context("test-b")

test_that("error works", {
  expect_error(class(plop))
  expect_error(class(iris))
  expect_error(class(1:10))
  expect_error(nrow(iris))
  expect_error(nrow(plop))
})

test_that("warning works", {
  expect_warning(warning("lol"))
  expect_warning(warning("pouet"))
  expect_warning(matrix(1:3, nrow = 4))
  expect_warning(matrix(1:2, nrow = 4))
})

test_that("message works", {
  expect_message(message("plop"))
  expect_message(class(iris))
  expect_message(message(1:10))
  expect_message(nrow(iris))
  expect_message(nrow(matrix(1:10)))
})
