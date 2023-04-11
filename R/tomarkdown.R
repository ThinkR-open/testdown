
#' testthat to bookdown
#'
#' This function turns the results of testthat into a bookdown. Each chapter is a context. The first page gives a summary of all the tests.
#'
#' @param project_name The name you want to give to the project. The default is `NULL`, which will be then be converted to `basename(here::here())`.
#' @param author The author of the test report. Default is set to `NULL`, then it will be skipped.
#' @param pkg The path to the package to document. Default is `here::here()`.
#' @param environment A name for the testing environment. Default is `here::here()`.
#' @param book_path The path to the bookdown output. Default is `"tests/testdown"`.
#' @param with_help Should the help appendix be added? Default is `TRUE`.
#' @param open Should the bookdown be opened once compiled? Default is `interactive()`.
#'
#' @export
#'
#' @importFrom devtools as.package test
#' @importFrom knitr kable knit
#' @importFrom rmarkdown render
#' @importFrom stats setNames na.omit
#' @importFrom utils browseURL packageVersion
#' @importFrom magrittr %>%
test_down <- function(
  project_name = NULL,
  author = NULL,
  pkg = here::here(),
  environment = here::here(),
  book_path = "tests/testdown",
  with_help = TRUE,
  open = interactive()
){
  entry_wd <- getwd()

  on.exit(
    setwd(
      entry_wd
    )
  )

  # Checking for context()
  # old_wd <- setwd(
  #   fs::path(
  #     normalizePath(pkg), "tests/testthat"
  #   )
  # )
  test_path <- fs::path(normalizePath(pkg), "tests/testthat")
  all_tests <- list.files(test_path, pattern = "*\\.R$", full.names = TRUE)
  all_tests_read <- unlist(lapply(as.character(all_tests), readLines))

  attempt::stop_if_any(
    all_tests_read,
    ~ grepl("^context\\(", .x),
    "Stopping `test_down()`. The use of `context()` in tests is disallowed when using `{testdown}` and has been deprecated in recent version of `{testthat}`.\nPlease remove them from your test files."
  )

  # browser()
  # old_wd <- setwd(
  #   normalizePath(pkg)
  # )

  book_path <- fs::path_abs(book_path)

  book_rmd <- fs::path(
    book_path,
    "index.Rmd"
  )

  .tr$df <- empty_tr()

  meta <- as.package(pkg)

  if (is.null(project_name)){
    meta$project_name <- basename(here::here())
  } else {
    meta$project_name <- project_name
  }

  if (dir.exists(book_path)){
    fs::dir_delete(book_path)
  }
  fs::dir_create(book_path)

  fs::file_copy(
    fs::dir_ls(
      system.file("booktemplate/", package = "testdown")
    ),
    book_path,
    overwrite = TRUE
  )

  replace_in_file(
    fs::path(book_path, "_bookdown.yml"),
    "teeest",
    meta$package
  )
  replace_in_file(
    # file.path(pkg, book_path, "index.Rmd"),
    file.path(book_path, "index.Rmd"),
    "XXXXXX",
    meta$package
  )

  a <- devtools::test(pkg, reporter = rmd_reporter)

  all_tests <- unique(.tr$df$context)

  # older <- setwd(
  #   fs::path(
  #     getwd(), "tests/testthat"
  #   )
  # )

  all_tests_read <- lapply(fs::path(test_path, as.character(all_tests)), readLines)

  names(all_tests_read) <- fs::path(test_path, as.character(all_tests)) #all_tests

  # browser()

  all_tests_read <- all_tests_read %>%
    purrr::imap(~{
      names(.x) <- paste0(normalizePath(.y), "#", 1:length(.x))
      res <- grep("expect\\_", .x, value = TRUE)
      gsub("^ +(.*)", "\\1", res)
    }) %>%
    unname() %>%
    unlist()

  were_skipped <- which(
    ! basename(names(all_tests_read)) %in% .tr$df$location
  )

  were_skipped_df <- build_were_skipped(
    were_skipped = all_tests_read[were_skipped]
  )


  # setwd(older)

  a <- do.call(
    rbind,
    lapply(a, summarize_one_test_results)
  )

  .tr$df$test_time <- as.character(.tr$df$test_time)
  .tr$df <- rbind(
    .tr$df,
    were_skipped_df
  )

  # browser()
  .tr$df <- split(.tr$df, .tr$df$context)
  .tr$df <- purrr::map(
    .tr$df, function(x){
      order_it(x)
    }
  ) %>% do.call(rbind, .)
  rownames(.tr$df) <- NULL


  .tr$df$test <- na_fill(.tr$df$test)

  write_in <- function(x = "\n", there = book_rmd){
    write(x, file = there, append = TRUE)
  }
  write_in()

  write_in(paste0("# `{testdown}` report for  `{", meta$package,"}` {-} \n"))

  write_first_page(
    write_in,
    meta,
    author,
    environment,
    .tr
  )

  write_in("# (PART) Results {-}")

  write_global_results(
    write_in,
    meta,
    author,
    a,
    .tr
  )

  by_fls <- .tr$df %>% split(.tr$df$file)
  names(by_fls) <- basename(names(by_fls))

  write_parts(
    write_in,
    by_fls
  )

  write_aggregated(
    write_in,
    .tr
  )

  if (with_help){
    write_help(
      write_in
    )
  }

  res <- rmarkdown::render(
    input = book_rmd
  )
  # browser()

  if (open){
    browseURL(res)
  }
  write_index_html(
    res
  )

  fs::file_delete(
    book_rmd
  )
  res
}

#' test_down(pkg = "../funk/")

