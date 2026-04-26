test_that("sanitize_kable_cell collapses blank lines (#4)", {
  expect_equal(
    sanitize_kable_cell("a\n\nb"),
    "a\nb"
  )
  expect_equal(
    sanitize_kable_cell("a\n\n\nb"),
    "a\nb"
  )
  expect_equal(
    sanitize_kable_cell("a\n   \n   \nb"),
    "a\nb"
  )
})

test_that("sanitize_kable_cell normalises Windows line endings", {
  expect_equal(
    sanitize_kable_cell("a\r\nb"),
    "a\nb"
  )
  expect_equal(
    sanitize_kable_cell("a\r\n\r\nb"),
    "a\nb"
  )
})

test_that("sanitize_kable_cell leaves a single newline alone", {
  expect_equal(
    sanitize_kable_cell("a\nb"),
    "a\nb"
  )
})

test_that("sanitize_kable_cell vectorises", {
  expect_equal(
    sanitize_kable_cell(c("a\n\nb", "c\nd", "e")),
    c("a\nb", "c\nd", "e")
  )
})

test_that("sanitize_kable_cell handles zero-length input", {
  expect_equal(sanitize_kable_cell(character()), character())
})
