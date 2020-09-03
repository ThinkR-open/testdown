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
