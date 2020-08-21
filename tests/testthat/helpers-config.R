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


## fake package
fakename <- sprintf(
  "%s%s",
  paste0(sample(letters, 10, TRUE), collapse = ""),
  gsub("[ :-]", "", Sys.time())
)

tpdir <- normalizePath(tempdir())
unlink(file.path(tpdir,fakename), recursive = TRUE)
pkg <- file.path(tpdir, fakename)
usethis::create_package(pkg, open = FALSE)
fs::file_copy(
  system.file("testhelpers/hello-world.R", package = "testdown"),
  fs::path(pkg, "R")
)
fs::file_copy(
  system.file("testhelpers/times.R", package = "testdown"),
  fs::path(pkg, "R")
)
fs::dir_copy(
  system.file("testhelpers/tests", package = "testdown"),
  pkg
)
devtools::document(pkg)
remotes::install_local(pkg)
# devtools::check(pkg)
# usethis::use_package("testthat")
