test_that("hello_world() works", {
  #' @description Testing that hello_world("colin") returns "Hello world, my name is colin!"
  expect_equal(hello_world("colin"), "Hello world, my name is colin!")
  #' @description Testing that hello_world("colin") returns a character vector
  expect_is(hello_world("colin"), "character")
  #' @description Testing that hello_world("colin") has World in it
  expect_match(hello_world("colin"), "World")
  #' @description Testing that hello_world("colin") has Hello in it
  expect_match(hello_world("colin"), "Hello")
})


test_that("hello_world() works with vectors", {
  #' @description Testing that hello_world("colin", "seb") returns two elements
  expect_error(hello_world("colin", "seb"), 2)
  #' @description Testing that hello_world(c("colin", "seb")) returns two elements
  expect_length(hello_world(c("colin", "seb")), 2)
  #' @description Testing that hello_world(LETTERS) returns 25 elements
  expect_length(hello_world(LETTERS), 25)
  #' @description Testing that hello_world(LETTERS) returns 26 elements
  expect_length(hello_world(LETTERS), 26)
})

