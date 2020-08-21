empty_tr <- function(){
  data.frame(
    context = character(0),
    test = character(0),
    expectation = character(0),
    description  = character(0),
    location = character(0),
    test_time = character(0),
    result = character(0),
    file = character(0),
    message = character(0),
    stringsAsFactors = FALSE
  )
}

.tr <- new.env(parent = emptyenv())
.tr$df <- empty_tr()

rmd_reporter <- R6::R6Class(
  classname = "CR",
  inherit = testthat::Reporter,
  #inherit = testthat_two_point_o$LocationReporter,
  public = list(
    ctx_list = c(),
    # Start reporter is called once, when the tests start
    start_reporter = function(){

      .tr$start_reporter <- Sys.time()
    },
    # Start test is called everytime a new test_that() is called
    start_test = function(context, test) {
      cat_if_verbose(
        sprintf(
          "Starting test %s", test
        )
      )
      .tr$current_block <- 1
    },
    # Start context is not called (AFAIK)
    start_context = function(){
      if (
        getOption("testdown-verbose", FALSE)
      ){
        cli::cat_rule(
          "Starting context"
        )
      }
    },

    initialize = function(...){
      super$initialize(...)
    },

    # add_result is called everytime an expect is called
    add_result = function(context, test, result) {
      cat_if_verbose(
        sprintf(
          "Starting test %s",
          deparse(result$expectation_calls[[1]])
        )
      )
      res <- df_results(
        result = result
      )

      .tr$df <- rbind(
        .tr$df,
        res
      )
    },

    end_test = function(context, test) {
      cat_if_verbose(
        "\n"
      )
      cat_if_verbose(
        sprintf("Test ended at %s", Sys.time())
      )
      cat_if_verbose(
        "\n"
      )
    },
    end_reporter = function(){
      return()
    }
  )
)
