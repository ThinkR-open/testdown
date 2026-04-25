test_that("test_down() does not error on a package whose only test_that is empty (#15)", {
  skip_if_not_installed("withr")

  pkg_root <- tempfile(pattern = "pkg-")
  dir.create(pkg_root)
  on.exit(unlink(pkg_root, recursive = TRUE), add = TRUE)
  file.copy(
    system.file("fake.package", package = "testdown"),
    pkg_root,
    recursive = TRUE
  )
  fake_pkg <- file.path(pkg_root, "fake.package")
  testthat_dir <- file.path(fake_pkg, "tests", "testthat")
  unlink(list.files(testthat_dir, full.names = TRUE))
  writeLines(
    c(
      'test_that("my_fun works", {',
      '})'
    ),
    file.path(testthat_dir, "test-empty.R")
  )

  book <- tempfile(pattern = "testdown-")

  expect_error(
    suppressMessages(suppressWarnings(
      withr::with_dir(fake_pkg, {
        test_down(pkg = ".", open = FALSE, book_path = book)
      })
    )),
    NA  # i.e., no error
  )
  expect_true(file.exists(book))
})
