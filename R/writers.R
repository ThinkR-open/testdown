write_first_page <- function(
  write_in,
  meta,
  author,
  environment,
  .tr
){

  if (is.null(author)){
    write_in(paste("> Performed on the:", Sys.time(),", using {testdown} version",packageVersion("testdown"),"\n"))
  } else {
    write_in(paste("> Performed on the:", Sys.time()," by ", author,", using {testdown} version",packageVersion("testdown") ,"\n"))
  }

  write_in("----\n")
  write_in()
  write_in("__Tested Package Information:__  \n")
  write_in(sprintf("+ __Title__ : %s", meta$title))
  write_in(sprintf("+ __Version__ : %s", meta$version))
  write_in(sprintf("+ __Description__ : %s", meta$description))
  write_in()
  write_in("----\n")
  write_in("__Project Information:__  \n")
  write_in(sprintf("+ __Project Name__ : %s\n", meta$project_name))
  write_in(sprintf("+ __Environment__ : %s\n", environment))
  write_in(sprintf("+ __Number of test file(s)__ : %s\n", length(unique(.tr$df$file))))
  write_in("----\n")
  write_in("__Result Overview:__  \n")
  write_in(sprintf("+ __Number of test(s)__ : %s\n", length(unique(na.omit(.tr$df$test)))))
  # write_in(sprintf("+ __Number of test(s) with error__ : %s\n", sum(.tr$df$result == 'error')))
  # write_in(sprintf("+ __Number of test(s) with skipped expectation(s)__ : %s\n", sum(.tr$df$result == 'skip')))
  write_in(sprintf("+ __Number of expectation(s)__ : %s\n", length(.tr$df$expectation)))
  write_in(sprintf("+ __Number of successful expectation(s)__ : %s\n", sum(.tr$df$result == 'success')))
  write_in(sprintf("+ __Number of failed expectation(s)__ : %s\n", sum(.tr$df$result == 'failure')))
  write_in(sprintf("+ __Number of errored expectation(s)__ : %s\n", sum(.tr$df$result == 'error')))
  write_in(sprintf("+ __Number of expectation(s) with warning(s)__ : %s\n", sum(.tr$df$result == 'warning')))
  write_in(sprintf("+ __Number of validated skip expectation(s)__ : %s\n",  sum(.tr$df$result %in% c('skip'))))
  write_in(sprintf("+ __Number of skipped expectation(s)__ : %s\n",  sum(.tr$df$result %in% c('was skipped'))))
  write_in("----\n")
  write_in()
  write_in("__Help:__  \n")
  write_in()
  write_in("<details><summary>Test session infrastructure (click to expand)</summary>")
  write_in()
  write_in("```{r echo = FALSE, comment = ''}")
  write_in("xfun::session_info()")
  write_in("```")
  write_in()
  write_in("</details>")
  write_in()
  write_in("<details><summary>Glossary (click to expand)</summary>")
  write_in()
  write_in("+ A __file__ is one test-*.R  file from the testthat folder.")
  write_in("+ A __test__ is one function call that starts with `test_that('', `.")
  #write_in("+ A __test with error__ is a `test_that('', ` in which one expectation has returned an error.")
  #write_in("+ A __test with skipped expectations__ is a `test_that('', ` in which one `skip` expectation has been validated.")
  write_in("+ An __expectation__ is a function call that starts with `expect`.")
  write_in("+ A __successful expectation__ is an expectation where the test is valid.")
  write_in("+ A __failed expectation__ is an expectation where the test is invalid.")
  write_in("+ An __expectation with warning__ is an expectation where the code of the test returned a warning.")
  write_in("+ An __errored expectation__ is an expectation where the code of the test returned an error (further expectations in the test are not run).")
  write_in("+ A __validated skip expectation__ is an expectation that starts with `skip`, and which has been validated (further expectations in the test are not run).")
  write_in("+ A __skipped expectation__ is an expectation which has not been run because it comes after an expectation with error or a validated skip expectation in the test.")
  write_in()
  write_in("</details>")
  write_in()
}

write_global_results <- function(
  write_in,
  meta,
  author,
  a,
  .tr
){
  write_in(paste("# Global results for package", meta$package,"{-} \n"))

  if (is.null(author)){
    write_in(paste("> Performed on the:", Sys.time(), "\n"))
  } else {
    write_in(paste("> Performed on the:", Sys.time()," by ", author,"\n"))
  }

  #browser()

  tst <- as.numeric(table(.tr$df$test)[a$test])

  tests_global <- data.frame(
    check.names = FALSE,
    stringsAsFactors = FALSE,
    "File" = sprintf("<a href='%s'>%s</a>", enurl(a$file), a$file),
    `Test` = a$test,
    `Expectations` = tst,
    Result = ifelse(a$failed | a$error | a$warning, "<span title='The test contains one or more error(s), failed expectation(s), or warning(s).'>&#10060;</span>", "<span title='No error, failed expectation, or warning.'>&#9989;</span>"),
    #`Was Skipped` = a$skipped,
    `Time Spent` = a$real
  )
  write_in(kable(tests_global, row.names = FALSE))
}

write_parts <- function(
  write_in,
  by_fls
){
  mapply(function(.x, .y) {
    table_to_insert <- .x
    table_to_insert$expectation <- htmltools::htmlEscape(table_to_insert$expectation)
    write_in("\n")
    write_in( paste( "#", basename(.y) ) )
    write_in("\n")
    write_in("## Summary {-}")
    write_in(sprintf("+ __Number of test(s)__ : %s\n", length(unique(na.omit(table_to_insert$test)))))
    # write_in(sprintf("+ __Number of test(s) with error__ : %s\n", sum(table_to_insert$result == 'error')))
    # write_in(sprintf("+ __Number of test(s) with skipped expectations__ : %s\n", sum(table_to_insert$result == 'skip')))
    write_in(sprintf("+ __Number of expectation(s)__ : %s\n", length(table_to_insert$expectation)))
    write_in(sprintf("+ __Number of successful expectation(s)__ : %s\n", sum(table_to_insert$result == 'success')))
    write_in(sprintf("+ __Number of failed expectation(s)__ : %s\n", sum(table_to_insert$result == 'failure')))
    write_in(sprintf("+ __Number of errored expectation(s)__ : %s\n", sum(table_to_insert$result == 'error')))
    write_in(sprintf("+ __Number of expectations with warning(s)__ : %s\n", sum(table_to_insert$result == 'warning')))
    write_in(sprintf("+ __Number of validated skip expectation(s)__ : %s\n", sum(table_to_insert$result == 'skip')))
    write_in(sprintf("+ __Number of skipped expectation(s)__ : %s\n", sum(table_to_insert$result %in% c("was skipped"))))

    table_to_insert$context <- NULL
    table_to_insert$message <- NULL
    table_to_insert$result <- gsub(
      "success",
      "<span title='This expectation is successful.'>&#9989; <font color='green'>Success</font></span>",
      table_to_insert$result
    )
    table_to_insert$result <- gsub(
      "failure",
      "<span title='This expectation is not successful.'>&#10060; <font color='red'>Failure</font></span>",
      table_to_insert$result
    )
    table_to_insert$result <- gsub(
      "error",
      "<span title='The code of this expectation returned an error.'>&#10060; <font color='red'>Error (test stopped)</font></span>",
      table_to_insert$result
    )
    table_to_insert$result <- gsub(
      "^skip$",
      "<span title='This expectation is a skip expectation, and it was validated.'>&#128260; <font color='blue'>Validated skip</font></span>",
      table_to_insert$result
    )
    table_to_insert$result <- gsub(
      "^was skipped$",
      "<span title='This expectation was not run, either because it comes after a validated skip or after an expectation that throw an error.'>&#128260; <font color='blue'>Skipped</font></span>",
      table_to_insert$result
    )
    table_to_insert$result <- gsub(
      "warning",
      "<span title='The code of this expectation returned a warning.'>&#9888;&#65039; <font color='orange'>Warning</font></span>",
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
        table_to_insert, escape = FALSE
      )
    )
    write_in()
  }, .x = by_fls, .y = names(by_fls))
}

write_aggregated <- function(
  write_in,
  .tr
){
  # Aggregate results for unsuccessful tests
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

  write_in("# Aggregated failures and errors {-}\n")
  write_in()
  write_in("> The expectation has not been validated, or the code has generated an error.")
  write_in()
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
  write_in()
  write_in("> A warning was thrown by these expectations.")
  write_in()
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
  write_in()
  write_in("> These expectations are either validated skip expectations, or they come after a validated skip expectations or an error and were not run.")
  write_in()
  if (nrow(skipped)){
    skipped <- skipped[, c("location", "test", "description", "expectation", "message")] %>%
      setNames(
        c("Location", "Test", "Description", "Expectation", "Message")
      )
    skipped$test <- na_fill(skipped$test)
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
}


write_help <- function(
  write_in
){
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

write_index_html <- function(
  res
){
  fs::file_create(
    fs::path(
      dirname(res),
      "index.html"
    )
  )
  write(
    file = fs::path(
      dirname(res),
      "index.html"
    ),
    sprintf('<head><meta http-equiv="refresh" content="0; URL=%s" /></head>', basename(res))
  )
}
