replace_in_file <- function(file, pattern, replacement){
  a <- readLines(file)
  a <- gsub(pattern, replacement, a)
  write(a, file)
}

df_results <- function(
  result,
  description = " "
){
  ref <- result$srcref
  if (is.null(ref)) {
    location <- "?#?:?"
  } else {
    location <- paste0(
      basename(
        attr(ref, "srcfile")$filename
      ),
      "#",
      ref[1]
    )
  }
  if (result$message != "success"){
    tmessage <- gsub("\n", " ", result$message)
  } else {
    tmessage <- "None"
  }
  # From testthat:::expectation_type

  status <- gsub("^expectation_", "", class(result)[[1]])
  call <- paste(result$srcref, collapse = "")
  call <- gsub(" {2,}", " ", call)
  context = basename(attr(result$srcref, "srcfile")$filename)
  if (location %in% .tr$df$location){
    return()
  }
  data.frame(
    stringsAsFactors = FALSE,
    context = basename(attr(result$srcref, "srcfile")$filename), # File == file
    test = result$test, # TEST == testthat("")
    expectation = call, # EXPECTATION == expect_*
    description = get_desc(result), # @description
    location = location, # LOCATION == line inside the file
    test_time = Sys.time(),  # test_time == Sys.time()
    result = status, # Result == "success" "failure"
    file = normalizePath(attr(ref, "srcfile")$filename),# file = file
    message = tmessage # output, or "none"
  )
}

#' @importFrom roxygen2 parse_text
#' @importFrom purrr map flatten keep
get_desc <- function(
  result
){
  fls <- readLines(normalizePath(attr(result$srcref, "srcfile")$filename))
  parsed <- parse_text(fls, NULL)
  tags <- map(parsed, "tags") %>% flatten()
  lines <- keep(
    tags,
    map(tags, "line") == result$srcref[1] - 1
  )
  if (!length(lines)){
    lines <- keep(
      tags,
      map(tags, "line") == result$srcref[1] - 2
    )
  }
  if (length(lines)){
    lines[[1]]$val
  } else {
    " "
  }

}

cat_if_verbose <- function(
  text
){
  if (
    getOption("testdown-verbose", FALSE)
  ){
    cli::cat_rule(
      text
    )
  }
}

# FROM https://github.com/r-lib/testthat/blob/55ccabaf8cce768636eb0f0e3d51abb3324dda69/R/reporter-list.R#L153
#' @importFrom utils getFromNamespace
summarize_one_test_results <- function(test) {
  test_results <- test$results
  nb_tests <- length(test_results)

  nb_failed <- nb_skipped <- nb_warning <- nb_passed <- 0L
  error <- FALSE

  if (nb_tests > 0) {
    # error reports should be handled differently.
    # They may not correspond to an expect_that() test so remove them
    last_test <- test_results[[nb_tests]]
    error <- getFromNamespace(
      "expectation_error",
      ns = "testthat"
    )(
      last_test
    )
    if (error) {
      test_results <- test_results[-nb_tests]
      nb_tests <- length(test_results)
    }

    nb_passed <- sum(
      vapply(
      test_results,
      getFromNamespace(
        "expectation_success",
        ns = "testthat"
        ),
      logical(1)
      )
    )
    nb_skipped <- sum(
      vapply(
      test_results,
      getFromNamespace(
        "expectation_skip",
        ns = "testthat"
        ),
      logical(1)
      )
    )
    nb_failed <- sum(
      vapply(
      test_results,
      getFromNamespace(
        "expectation_failure",
        ns = "testthat"
        ),
      logical(1)
      )
    )
    nb_warning <- sum(
      vapply(
      test_results,
      getFromNamespace(
        "expectation_warning",
        ns = "testthat"
        ),
      logical(1)
      )
    )
  }

  context <- if (length(test$context) > 0) test$context else ""

  res <- data.frame(
    file = test$file,
    context = context,
    test = test$test,
    nb = nb_tests,
    failed = nb_failed,
    skipped = as.logical(nb_skipped),
    error = error,
    warning = nb_warning,
    user = test$user,
    system = test$system,
    real = test$real,
    stringsAsFactors = FALSE
  )

  # Added at end for backward compatibility
  res$passed <- nb_passed

  # Cannot easily add list columns in data.frame()
  res$result <- list(test_results)
  res
}

enurl <- function(txt){
  txt <- gsub("\\.", "-", txt)
  txt <- tolower(txt)
  paste0(txt, ".html")
}

# https://stackoverflow.com/questions/10554741/fill-in-data-frame-with-values-from-rows-above
na_fill <- function(x) {
  for(i in seq_along(x)[-1]) if(is.na(x[i])) x[i] <- x[i-1]
  x
}

order_it <- function(table_to_insert){
  orders <- gsub(
    ".*#(.*)", "\\1", table_to_insert$location
  ) %>% as.numeric()
  table_to_insert[
    order(orders),
  ]
}
