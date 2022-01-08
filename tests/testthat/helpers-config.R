cli::cat_rule("Setting config")
### lib
library(withr)
library(usethis)
### Funs
expect_file_exists <- function(fls) {

  act <- quasi_label(rlang::enquo(fls), arg = "fls")

  act$val <- file.exists(fls)
  expect(
    isTRUE(act$val),
    sprintf("File %s doesn't exist.", fls)
  )

  invisible(act$val)
}

expect_path_exists <- function(dir) {

  act <- quasi_label(rlang::enquo(dir), arg = "dir")

  act$val <- dir.exists(dir)
  expect(
    isTRUE(act$val),
    sprintf("dir %s doesn't exist.", dir)
  )

  invisible(act$val)
}
