test_that("times() works", {
  #' @description Testing that times(1, 3) returns 3
  expect_equal(times(1, 3), 3)
  #' @description Testing that times(1, "2") fails
  expect_error(times(1, "3"))
  #' @description Testing that times(1, "a") fails
  expect_error(times(1, "a"))
  #' @description Testing that times(1, 1:10) returns one number
  expect_length(times(1, 1:10), 1)
  #' @description Testing that times(1, 1:10) returns a vector
  expect_is(times(1, 1:10),"vector")
  #' @description Testing that times(1, "3") returns 3
  expect_equal(times(1, "3"), 3)
  #' @description Testing that times(1, 1.5) returns 1.5
  expect_equal(times(1, 1.5), 1.5)
})


test_that("times() works with matrix", {
  #' @description Testing that the matrix size is 10 for times(1:3, matrix(1:10))
  expect_length(times(1:3, matrix(1:10)), 10)
  #' @description Testing that the matrix size is 10 for times(1:3, matrix(1:15))
  expect_length(times(1:3, matrix(1:15)), 10)
  #' @description Testing that the matrix size is 10 for times(1:2, matrix(1:10))
  expect_length(times(1:2, matrix(1:10)), 10)
})

test_that("times() works with {agivenpackage}", {
  #' @description Testing that the length of iris is 5
  expect_length(iris, 5)
  # Example of a skipped test
  skip_if_not_installed("agivenpackage")
  #' @description Testing that times works with agivenpackage
  res <- agivenpackage::fun(times(1, 3))
  expect_true(res$res)
  expect_length(iris, 5)
})
