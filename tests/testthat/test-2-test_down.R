# context("Verify testdown work")

# herewd <- getwd()
# Copy fake package
tmppkg <- tempfile(pattern = "pkg-")
dir.create(tmppkg)
file.copy(
  system.file("fake.package", package = "testdown"),
  tmppkg,
  recursive = TRUE
)

# Create testdown
out.dir <- tempfile(pattern = "testdown-")
unlink(out.dir)
out <- test_down(
  pkg = file.path(tmppkg, "fake.package"),
  open = FALSE,
  book_path = out.dir
)
browseURL(out)

# Readlines with tests errors
lines_escapehtml <- readLines(file.path(out.dir, "test-golem_utils_ui.html"))
# Correct number of errors
errors_ok <- grep("<li><p><strong>Number of errored expectation(s)</strong> : 2</p></li>", lines_escapehtml, fixed = TRUE)
# HTML code correctly escaped
same_line_begin <- grep(
  'as.character(with_red_star(',
     lines_escapehtml, fixed = TRUE)
same_line_end <- grep(
  'Enter your name here',
  lines_escapehtml, fixed = TRUE)
# Result error in table
table_errors <- grep("Error (test stopped)", lines_escapehtml, fixed = TRUE)

test_that("test_down escapes and find tests errors", {
  expect_true(
    file.exists(
      file.path(
        out.dir,
        "test-golem_utils_ui.html"
      )
    )
  )
  expect_true(length(errors_ok) == 1)
  expect_true(same_line_begin == same_line_end)
  expect_true(length(table_errors) == 2)
})

# Readlines with tests success
lines_escapehtml <- readLines(file.path(out.dir, "test-my_mean.html"))
# Correct number of errors
success_ok <- grep("<li><p><strong>Number of successful expectation(s)</strong> : 2</p></li>", lines_escapehtml, fixed = TRUE)
# Result success in table
table_success <- grep('Success', lines_escapehtml, fixed = TRUE)
table_success_expectation_same <- grep('This expectation is successful', lines_escapehtml, fixed = TRUE)

test_that("test_down escapes and find tests errors", {
  expect_true(
    file.exists(
      file.path(
        out.dir,
        "test-my_mean.html"
      )
    )
  )
  expect_true(length(success_ok) == 1)
  expect_true(all(table_success_expectation_same %in% table_success))
})

# setwd(herewd)
unlink(tmppkg, recursive = TRUE)

