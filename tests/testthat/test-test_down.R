context("Verify testdown work")

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
#
test_that("test_down works", {
  expect_true(
    file.exists(
      file.path(
        out.dir,
        "testdown-report-for-dummypackage.html"
      )
    )
  )
})
