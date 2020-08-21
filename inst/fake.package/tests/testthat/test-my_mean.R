test_that("my_mean works", {
  expect_equal(my_mean(c(1, 6)), 3.5)
  expect_equal(my_mean(c(1, 6, NA)), 3.5)
})
