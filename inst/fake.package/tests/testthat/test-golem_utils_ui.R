
test_that("with_red_star works", {
  expect_equal(
    as.character(with_red_star("Enter your name here")),
    '<span>Enter your name here<span style="color:red">*</span></span>'
  )
})

test_that("list_to_li works", {
  expect_equal(
    as.character(list_to_li(c("a","b"))),
    "<li>a</li>\n<li>b</li>")
})
