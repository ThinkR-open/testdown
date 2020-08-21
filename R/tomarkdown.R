
#' testthat to bookdown
#'
#' This function turns the results of testthat into a bookdown. Each chapter is a context. The first page gives a summary of all the tests.
#'
#' @param project_name The name you want to give to the project. The default is `NULL`, which will be then be converted to `basename(here::here())`.
#' @param author The author of the test report. Default is set to `NULL`, then it will be skipped.
#' @param pkg The path to the package to document. Default is `here::here()`
#' @param book_path The path to the bookdown output. Default is `"tests/testdown"`.
#' @param with_help Should the help appendix be added? Default is `TRUE`.
#' @param open Should the bookdown be opened once compiled? Default is `interactive()`.
#'
#' @export
#'
#' @importFrom devtools as.package test
#' @importFrom knitr kable knit
#' @importFrom rmarkdown render
#' @importFrom stats setNames
#' @importFrom utils browseURL packageVersion
#' @importFrom magrittr %>%
test_down <- function(
  project_name = NULL,
  author = NULL,
  pkg = here::here(),
  book_path = "tests/testdown",
  with_help = TRUE,
  open = interactive()
){

  old_wd <- setwd(
    normalizePath(pkg)
  )
  on.exit(
    setwd(
      old_wd
    )
  )

  book_rmd <- fs::path(
    pkg,
    book_path,
    "index.Rmd"
  )

  .tr$df <- empty_tr()

  meta <- as.package(".")

  if (is.null(project_name)){
    meta$project_name <- basename(here::here())
  } else {
    meta$project_name <- project_name
  }

  output_folder <- fs::path_abs(
    fs::path(
      pkg,
      book_path
    )
  )

  if (dir.exists(output_folder)){
    fs::dir_delete(output_folder)
  }
  fs::dir_create(output_folder)

  fs::file_copy(
    fs::dir_ls(
      system.file("booktemplate/", package = "testdown")
    ),
    output_folder,
    overwrite = TRUE
  )

  replace_in_file(
    fs::path(pkg, "tests/testdown", "_bookdown.yml"),
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

  older <- setwd(
    fs::path(
      getwd(), "tests/testthat"
    )
  )

  all_tests_read <- lapply(as.character(all_tests), readLines)

  names(all_tests_read) <- all_tests

  all_tests_read <- all_tests_read %>%
    purrr::imap(~{
      names(.x) <- paste0(normalizePath(.y), "#", 1:length(.x))
      res <- grep("expect_*", .x, value = TRUE)
      gsub("^ +(.*)", "\\1", res)
    }) %>%
    unname() %>%
    unlist()

  were_skipped <- which(
    ! basename(names(all_tests_read)) %in% .tr$df$location
  )
  were_skipped <- all_tests_read[were_skipped]
  #get_desc()
  #all_tests_description <- lapply(as.character(all_tests), readLines)
  #were_skipped_description <-

  were_skipped_df <- data.frame(
    row.names = NULL,
    stringsAsFactors = FALSE,
    context = gsub(
      "(.*)#.*",
      "\\1",
      basename(
        names(were_skipped)
      )
    ),
    test = NA,
    expectation = were_skipped,
    description = "NA (was skipped)",
    location = basename(
      names(were_skipped)
    ),
    test_time = "NA (was skipped)",
    result  = "was skipped",
    file =  gsub(
      "(.*)#.*",
      "\\1",
      names(were_skipped)
    ),
    message = "NA (was skipped)"
  )

  #browser()
  setwd(older)

  write_in <- function(x = "\n", there = book_rmd){
    write(x, file = there, append = TRUE)
  }
  write_in()
  write_in(paste0("# `{testdown}` report for  `{", meta$package,"}` {-} \n"))
  if (is.null(author)){
    write_in(paste("> Performed on the:", Sys.time(),", using {testdown} version",packageVersion("testdown"),"\n"))
  } else {
    write_in(paste("> Performed on the:", Sys.time()," by ", author,", using {testdown} version",packageVersion("testdown") ,"\n"))
  }

  write_in("----\n")
  write_in("__Project Information:__  \n")
  write_in(sprintf("+ __Project Name__ : %s\n", meta$project_name))
  write_in(sprintf("+ __Environment__ : %s\n", here::here()))
  write_in(sprintf("+ __Number of test file(s)__ : %s\n", length(unique(.tr$df$file))))
  write_in(sprintf("+ __Total number of tests__ : %s\n", length(unique(.tr$df$test))))
  write_in(sprintf("+ __Total number of test(s) with error(s)__ : %s\n", sum(.tr$df$result == 'error')))
  write_in(sprintf("+ __Total number of test(s) with skipped expectations__ : %s\n", sum(.tr$df$result == 'skip')))
  write_in(sprintf("+ __Total number of expectation(s)__ : %s\n", length(unique(.tr$df$expectation)) + length(were_skipped)))
  write_in(sprintf("+ __Total number of successful expectation(s)__ : %s\n", sum(.tr$df$result == 'success')))
  write_in(sprintf("+ __Total number of failed expectation(s)__ : %s\n", sum(.tr$df$result == 'failure')))
  write_in(sprintf("+ __Total number of expectations with warning(s)__ : %s\n", sum(.tr$df$result == 'warning')))
  write_in(sprintf("+ __Total number of skipped expectation(s)__ : %s\n", length(were_skipped)))
  write_in()
  write_in("__Tested Package Information:__  \n")
  write_in(sprintf("+ __Title__ : %s", meta$title))
  write_in(sprintf("+ __Version__ : %s", meta$version))
  write_in(sprintf("+ __Description__ : %s", meta$description))
  write_in()
  write_in("<details><summary>Test session infrastructure</summary>")
  write_in()
  write_in("```{r echo = FALSE, comment = ''}")
  write_in("xfun::session_info()")
  write_in("```")
  write_in()
  write_in("</details>")
  write_in()
  write_in()
  write_in("# (PART) Results {-}")

  write_in(paste("# Global results for package", meta$package,"{-} \n"))

  if (is.null(author)){
    write_in(paste("> Performed on the:", Sys.time(), "\n"))
  } else {
    write_in(paste("> Performed on the:", Sys.time()," by ", author,"\n"))
  }

  a <- do.call(
    rbind,
    lapply(a, summarize_one_test_results)
  )

  #names(.tr$df)
  #

  .tr$df$test_time <- as.character(.tr$df$test_time)
  .tr$df <- rbind(
    .tr$df,
    were_skipped_df
  )


  tests_global <- data.frame(
    check.names = FALSE,
    stringsAsFactors = FALSE,
    "File" = sprintf("<a href='%s'>%s</a>", enurl(a$file), a$file),
    `Test` = a$test,
    `Expectations` = a$nb,
    Result = ifelse(a$failed | a$error, "&#10060;", "&#9989;"),
    `Was Skipped` = a$skipped,
    `Time Spent` = a$real
  )
  write_in(kable(tests_global, row.names = FALSE))

  # Aggregate results for unsuccessful tests
  #failed <- filter(.tr$df, result %in% c("failure", "error"))
  failed <- .tr$df[.tr$df$result %in% c("failure", "error"), ]
  failed$file <- NULL
  failed$result <- NULL


  # Aggregate warnings for unsuccessful tests
  warnings <- .tr$df[.tr$df$result == "warning", ]
  warnings$file <- NULL
  warnings$result <- NULL

  # Aggregate skipped for unsuccessful tests
  skipped <- .tr$df[.tr$df$result %in% c("skip", "was skipped"), ]
  skipped$file <- NULL
  skipped$result <- NULL

  by_fls <- .tr$df %>% split(.tr$df$file)
  names(by_fls) <- basename(names(by_fls))



  mapply(function(.x, .y) {
    table_to_insert <- .x
    write_in("\n")
    write_in( paste( "#", basename(.y) ) )
    write_in("\n")
    write_in("## Summary {-}")
    write_in(sprintf("+ __Number of test(s)__ : %s\n", length(unique(table_to_insert$test))))
    write_in(sprintf("+ __Number of test(s) with error__ : %s\n", sum(table_to_insert$result == 'error')))
    write_in(sprintf("+ __Number of test(s) with skipped expectations__ : %s\n", sum(table_to_insert$result == 'skip')))
    write_in(sprintf("+ __Number of expectation(s)__ : %s\n", length(unique(table_to_insert$expectation))))
    write_in(sprintf("+ __Number of successful expectation(s)__ : %s\n", sum(table_to_insert$result == 'success')))
    write_in(sprintf("+ __Number of failed expectation(s)__ : %s\n", sum(table_to_insert$result == 'failure')))
    write_in(sprintf("+ __Number of expectations with warning(s)__ : %s\n", sum(table_to_insert$result == 'warning')))
    write_in(sprintf("+ __Number of skipped expectation(s)__ : %s\n", sum(table_to_insert$result == "was skipped")))

    table_to_insert$context <- NULL
    table_to_insert$message <- NULL
    table_to_insert$result <- gsub(
      "success",
      "&#9989; <font color='green'>Success</font>",
      table_to_insert$result
    )
    table_to_insert$result <- gsub(
      "failure",
      "&#10060; <font color='red'>Failure</font>",
      table_to_insert$result
    )
    table_to_insert$result <- gsub(
      "error",
      "&#10060; <font color='red'>Error (test stopped)</font>",
      table_to_insert$result
    )
    table_to_insert$result <- gsub(
      "^skip$",
      "&#128260; <font color='blue'>Skip</font>",
      table_to_insert$result
    )
    table_to_insert$result <- gsub(
      "^was skipped$",
      "&#128260; <font color='blue'>Was Skipped</font>",
      table_to_insert$result
    )
    table_to_insert$result <- gsub(
      "warning",
      "&#9888;&#65039; <font color='orange'>Warning</font>",
      table_to_insert$result
    )
    table_to_insert <- table_to_insert[
      , c("test", "description", "expectation", "result", "location", "test_time")
    ]
    orders <- gsub(
      ".*#(.*)", "\\1", table_to_insert$location
    ) %>% as.numeric()
    table_to_insert <- table_to_insert[
      order(orders),
    ]
    table_to_insert$test <- na_fill(table_to_insert$test)
    names(table_to_insert) <- tools::toTitleCase(names(table_to_insert))

    for (i in names(table_to_insert)){
      table_to_insert[, i] <- gsub("\\$", "&#36;", table_to_insert[, i])
    }

    write_in()
    write_in("## Details {-}")
    write_in()
    write_in(
      kable(
        row.names = FALSE,
        table_to_insert
      )
    )
    write_in()
  }, .x = by_fls, .y = names(by_fls))


  write_in("# Aggregated failures and errors {-}\n")

  if (nrow(failed)){
    failed <- failed[, c("location", "test", "description", "expectation", "message")] %>%
      setNames(
        c("Location", "Test", "Description", "Expectation", "Message")
      )
    write_in(
      kable(
        row.names = FALSE,
        failed
      )
    )
  } else {
    write_in()
    write_in("No failure or error found")
    write_in()
  }



  write_in()
  write_in("# Aggregated warnings {-}\n")
  if (nrow(warnings)){
    warnings <- warnings[, c("location", "test", "description", "expectation", "message")] %>%
      setNames(
        c("Location", "Test", "Description", "Expectation", "Message")
      )
    write_in(
      kable(
        row.names = FALSE,
        warnings
      )
    )
  } else {
    write_in()
    write_in("No warning found")
    write_in()
  }

  write_in("# Aggregated skipped {-}\n")

  if (nrow(skipped)){
    skipped <- skipped[, c("location", "test", "description", "expectation", "message")] %>%
      setNames(
        c("Location", "Test", "Description", "Expectation", "Message")
      )
    write_in(
      kable(
        row.names = FALSE,
        skipped
      )
    )
  } else {
    write_in()
    write_in("No skipped found")
    write_in()
  }


  if (with_help){
    write_in("# (PART) Appendix {-}")
    write_in("# How to read this report {-} \n")
    write_in(
      readLines(
        system.file("testdownhelp.md", package = "testdown")
      )
    )
    write_in("### Appendix {-} \n")
    write_in()
    write_in("This page")
    write_in()
  }

  res <- render(
    file.path(
      pkg,
      book_path,
      "index.Rmd"
    )
  )
  #knit(file.path(pkg, "tests/testdown", "index.Rmd"))
  if (open){
    browseURL(res)
  }

  fs::file_delete(
    fs::path(
      pkg,
      book_path,
      "index.Rmd"
    )
  )
  res
}

#' test_down(pkg = "../funk/")

