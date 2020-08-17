### lib
library(withr)
library(usethis)
### Funs
remove_file <- function(path){
  if (file.exists(path)) unlink(path, force = TRUE)
}

remove_files <- function(path, pattern = NULL){
  fls <- list.files(
    path, pattern, full.names = TRUE, recursive = TRUE
  )
  if (length(fls) >0){
    res <- lapply(fls, function(x){
      if (file.exists(x)) unlink(x, force = TRUE)
    })
  }
}

expect_exists <- function(fls) {

  act <- quasi_label(rlang::enquo(fls), arg = "fls")

  act$val <- file.exists(fls)
  expect(
    isTRUE(act$val),
    sprintf("File %s doesn't exist.", fls)
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
browser()
fs::file_copy(
  "hello-world.R",
  fs::path(pkg, "R")
)
fs::file_copy(
  "times.R",
  fs::path(pkg, "R")
)
fs::dir_copy(
  "tests",
  pkg
)
devtools::document(pkg)
devtools::check(pkg)
usethis::use_package("testthat")
usethis::use_package("testdown")
