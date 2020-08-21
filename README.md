
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

`{testdown}`
============

The goal of `{testdown}` is to generate a bookdown report of
`{testthat}` results

Installation
------------

You can install the dev version of `{testdown}` from GitHub with:

    remotes::install_github("ThinkR-open/testdown")

About
-----

This package has two exported functions:

### `test_down()`

This function turns the `{testthat}` results into a `{bookdown}` report.
It takes:

-   A `project_name`, which is the name you want to give to the project.
    The default is `NULL`, which will then be converted to
    `basename(here::here())`.
-   An `author`, if you want your report to be have an author name.
    Default is NULL, and then the report won’t have any name on it.
-   `pkg`, the path to the package that will be documented. Default is
    `here::here()`
-   `book_path`, the folder where you want the `{bookdown}` report to be
    created. Default is `"tests/testdown"`.
-   `with_help` Should the help appendix be added? Default is `TRUE`.
-   `open` Should the report be opened once compiled? Default is
    `interactive()`.

### `test_down_example()`

This function will compile the report for an example package contained
inside `{testdown}`.

Custom `{testdown}` roxygen tags
--------------------------------

You can add a `description` to your tests using the roxygen tag
`@description`. Note that this test **must** be on the line just before
the expectation.

Here is an example:

    test_that("hello_world() works", {
      
      #' @description Testing that hello_world("colin") returns "Hello world, my name is colin!"
      expect_equal(hello_world("colin"), "Hello world, my name is colin!")
      
      #' @description Testing that hello_world("colin") returns a character vector
      expect_is(hello_world("colin"), "character")
      
      #' @description Testing that hello_world("colin") has World in it
      expect_match(hello_world("colin"), "World")
      
      #' @description Testing that hello_world("colin") has Hello in it
      expect_match(hello_world("colin"), "Hello")
    })

Writing custom expectation
--------------------------

For reliable results, all expectations should start by `expect`, notably
if you need to count the skipped tests. In other words, the skipped
expectations count relies on counting all the functions starting with
`expect`, so naming custom expectation differently will prevent this
count from being correct.

Known limitations
-----------------

`{testdown}` report relies on `{testthat}` and the results are based on
these outputs.

Here are known example of results that will be discarded by `{testthat}`
and hence will make `{testdown}` behave in a weird way.

-   Using `{withr}`

<!-- -->

    test_that("Files exist", {
      with_dir(
        "new/dir",
        {
          #' @description Checking that style.css is created
          expect_file_exists("tests/testdown/style.css")
          #' @description Checking that _bookdown.yml is created
          expect_file_exists("tests/testdown/_bookdown.yml")
          #' @description Checking that _output.yml is created
          expect_file_exists("tests/testdown/_output.yml")
        }
      )
    })

As testthat doesn’t count the expectations from `with_dir`, this will
make the `{testdown}` result weird. Always add the expectations at the
top level of your test\_that.

-   Loops

Same goes with for loops:

    for (i in names(df)){
        #' @description Checking the names of the output are correct
        expect_true(
          i %in% c(
            "context",
            "test",
            "expectation",
            "description",
            "location",
            "test_time",
            "result",
            "file",
            "message"
          )
        )
      }

If ever you use this format, `{testthat}` won’t catch the tests, so they
won’t be reported.

-   HTML expectations

As testdown render the text straight in html, if your expecatation
contains html, it will break the rendering. For example, the following
test will break the rendering:

    expect_match(
      tag, 
      "<h2>this</h2>"
    )

Sponsor
-------

The development of this package has been sponsored by:

<a href = "https://www.servier.fr/"><img src = "readmefigs/servier.png"></img></a>

CoC
---

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
