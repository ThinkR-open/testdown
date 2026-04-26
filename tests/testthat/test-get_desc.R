# Tests for get_desc() — covers issues #18 (multiline / non-immediate
# description) and an indirect impact on the table rendering.
#
# We exercise get_desc() by emulating the result list it consumes.
# A `result` is a named list with `$srcref` (numeric line refs +
# `srcfile` attribute pointing at a real file).

make_fake_result <- function(file, line) {
  ref <- c(line, 1L, line, 1L)
  attr(ref, "srcfile") <- list(filename = file)
  list(srcref = ref)
}

write_fixture <- function(lines) {
  path <- tempfile(fileext = ".R")
  writeLines(lines, path)
  path
}

test_that("get_desc() collapses newlines inside a description into spaces (#18)", {
  path <- write_fixture(c(
    "test_that(\"two-line desc\", {",
    "  #' @description This description spans",
    "  #' two lines",
    "  expect_true(TRUE)",
    "})"
  ))
  on.exit(unlink(path), add = TRUE)
  res <- make_fake_result(path, line = 4L)
  desc <- get_desc(res)
  expect_false(grepl("\n", desc, fixed = TRUE),
               info = "get_desc() must not return a newline (would break the kable cell)")
  expect_match(desc, "spans two lines", fixed = TRUE)
})

test_that("get_desc() finds a description that is more than two lines above (#18)", {
  path <- write_fixture(c(
    "test_that(\"distant desc\", {",
    "  #' @description Top-level description",
    "  some_intermediate_call()",
    "  another_call()",
    "  expect_true(TRUE)",
    "})"
  ))
  on.exit(unlink(path), add = TRUE)
  res <- make_fake_result(path, line = 5L)
  desc <- get_desc(res)
  expect_match(desc, "Top-level description", fixed = TRUE)
})

test_that("get_desc() does not bleed a description across test_that blocks (self-review of #18)", {
  path <- write_fixture(c(
    "test_that(\"first test with desc\", {",
    "  #' @description Description for the FIRST test only",
    "  expect_true(TRUE)",
    "})",
    "",
    "test_that(\"second test without desc\", {",
    "  expect_true(TRUE)",
    "})"
  ))
  on.exit(unlink(path), add = TRUE)
  res <- make_fake_result(path, line = 7L) # second test's expect line
  desc <- get_desc(res)
  expect_equal(desc, " ",
    info = "second test_that has no description and must NOT inherit the first test's")
})

test_that("get_desc() returns ' ' when there is no description tag", {
  path <- write_fixture(c(
    "test_that(\"no desc\", {",
    "  expect_true(TRUE)",
    "})"
  ))
  on.exit(unlink(path), add = TRUE)
  res <- make_fake_result(path, line = 2L)
  desc <- get_desc(res)
  expect_equal(desc, " ")
})
