## fake package
# fakename <- sprintf(
#   "%s%s",
#   paste0(sample(letters, 10, TRUE), collapse = ""),
#   gsub("[ :-]", "", Sys.time())
# )
#
# tpdir <- tempfile("dir-")
# dir.create(tpdir)
# # unlink(file.path(tpdir,fakename), recursive = TRUE)
# pkg <- file.path(tpdir, fakename)
# withr::with_dir(tpdir, {
#   usethis::create_package(pkg, open = FALSE)
#   fs::file_copy(
#     system.file("testhelpers/hello-world.R", package = "testdown"),
#     fs::path(pkg, "R")
#   )
#   fs::file_copy(
#     system.file("testhelpers/times.R", package = "testdown"),
#     fs::path(pkg, "R")
#   )
#   fs::dir_copy(
#     system.file("testhelpers/tests", package = "testdown"),
#     pkg
#   )
#   devtools::document(pkg)
# })
#remotes::install_local(pkg)
# devtools::check(pkg)
# usethis::use_package("testthat")

# Copy fake package
tpdir <- tempfile("dir-")
dir.create(tpdir)
# unlink(file.path(tpdir,fakename), recursive = TRUE)
# pkg <- file.path(tpdir, fakename)
# pkg <- tempfile(pattern = "pkg-")
# dir.create(pkg)
file.copy(
  system.file("fake.package", package = "testdown"),
  tpdir,
  recursive = TRUE
)
pkg <- file.path(tpdir, 'fake.package')
browseURL(pkg)

# test ----
withr::with_dir(pkg, {
  test_that("testdown works", {
    # browser()
    # oldir <- setwd(pkg)
    res <- test_down(
      project_name = "testthat testdown",
      author = "Colin Fay",
      pkg = pkg,
      open = FALSE
    )
    #' @description Checking that the report is created
    expect_path_exists(
      dirname(res)
    )
    # setwd(oldir)
  })


  test_that("Files exist", {
    # oldir <- setwd(pkg)
    #' @description Checking that style.css is created
    expect_file_exists("tests/testdown/style.css")
    #' @description Checking that _bookdown.yml is created
    expect_file_exists("tests/testdown/_bookdown.yml")
    #' @description Checking that _output.yml is created
    expect_file_exists("tests/testdown/_output.yml")
    # setwd(oldir)
  })

  test_that("Configs are correct", {
    #browser()
    # oldir <- setwd(pkg)
    font_m <- yaml::read_yaml(
      "tests/testdown/_bookdown.yml"
    )
    #' @description The config filename should be the name of the package
    expect_equal(
      font_m$book_filename,
      basename(pkg)
    )
    #' @description Merged files should be deleted
    expect_true(
      font_m$delete_merged_file
    )
    #' @description chapter_name should be "Test File "
    expect_equal(
      font_m$language$ui$chapter_name,
      "Test File "
    )

    font_m <- yaml::read_yaml(
      "tests/testdown/_output.yml"
    )
    #' @description toc_depth should stay 1
    expect_equal(
      font_m$`bookdown::gitbook`$toc_depth,
      1
    )
    #' @description css should be style.css
    expect_equal(
      font_m$`bookdown::gitbook`$css,
      "style.css"
    )
    #' @description toc$before should contain '{testdown} report'
    expect_match(fixed = TRUE,
                 font_m$`bookdown::gitbook`$config$toc$before,
                 "{testdown} report"
    )
    #' @description toc$after should contain link to the {testdown}
    expect_match(fixed = TRUE,
                 font_m$`bookdown::gitbook`$config$toc$after,
                 "https://github.com/ThinkR-open/testdown"
    )
    # setwd(oldir)

  })

})

unlink(tpdir, recursive = TRUE)
