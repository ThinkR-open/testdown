book_example <- tempfile(pattern = "ex")
dir.create(book_example)
file.copy(
  system.file("fakepkg", package = "testdown"),
  book_example,
  recursive = TRUE
)
pkg_example <- file.path(book_example, "fakepkg")
# browseURL(pkg_example)

skip_if_not(interactive())

test_that("test_down_example works", {
  withr::with_dir(pkg_example, {
    expect_error(test_down_example(pkg = pkg_example), regexp = NA)
  })
})

all_files <- c(
  "_bookdown.yml", "_output.yml", "404.html",
  "aggregated-failures-and-errors.html",
  "aggregated-skipped.html", "aggregated-warnings.html",
  "global-results-for-package-fakepkg.html",
  "how-to-read-this-report.html", "index.html",
  "style.css", "test-hello-world.html",
  "test-times.html", "testdown-report-for-fakepkg.html")
for (the_file in all_files) {
  test_that(paste(the_file, "exists"), {
    expect_file_exists(
      file.path(pkg_example, "tests", "testdown", the_file)
    )
  })
}

unlink(book_example, recursive = TRUE)
