test_that("testdown works", {
  oldir <- setwd(pkg)
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
  setwd(oldir)
})


test_that("Files exist", {
  oldir <- setwd(pkg)
  #' @description Checking that style.css is created
  expect_file_exists("tests/testdown/style.css")
  #' @description Checking that _bookdown.yml is created
  expect_file_exists("tests/testdown/_bookdown.yml")
  #' @description Checking that _output.yml is created
  expect_file_exists("tests/testdown/_output.yml")
  setwd(oldir)
})

test_that("Configs are correct", {
  #browser()
  oldir <- setwd(pkg)
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
  setwd(oldir)

})



