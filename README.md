
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
-   `environment`, a name for the testing environment. Default is
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

How to read {testdown} reports
------------------------------

### Glossary

#### Scopes of tests

There are three scopes when it comes to tests:

-   “Test files”, which are the files included inside the
    `test/testthat/` folder, and which contain a testing infrastructure.
    Each file contains one ore more test(s).

-   “Tests”, which start with `test_that("",`. Each test contains one or
    more expectation(s).

-   “Expectations”, which (usually) start with `expect_` (note that
    `skip_` functions are also considered as expectations). An
    expectation only contains one statement. If you put a roxygen tag
    `@description` **just before your expectation**, it will be
    displayed in the testing results (otherwise the ‘Description’ column
    will be blank).

To sum up, Test files contains one or more test(s), which contain one or
more expectation(s).

#### Expectations status

-   ✅ <font color='green'>Success</font>: Passed (the expectation is
    met).

-   ❌ <font color='red'>Failure</font>: Failed (the expectation is not
    met).

-   ⚠️ <font color='orange'>Warning</font>: The expectation returned a
    warning.

-   ❌ <font color='red'>Error (test stopped)</font>: the expectation has
    returned an error, and the current test was stopped (i.e further
    expectations in the current test will not be launched).

-   🔄 <font color='blue'>Skip</font>: This expectation has validated a
    “skip” expectation for the current test.

-   🔄 <font color='blue'>Was Skipped</font>: The expectation has been
    skipped and is reported as a ‘skipped expectation’, due to a
    `skip_if` or an error previously in the test. Note that when an
    expectation is skipped, the Description, and Test time are not
    retrieved.

### First page

The index offers a summary of:

-   When the tests were run

-   The package (Title, Version, and Description of the package)

-   The project

    -   Name of the project
    -   Environment the tests were run into (aka the directory)
    -   Details about the test results

-   Results overview

-   The testing infrastructure

    -   R version
    -   Operating system
    -   Locale
    -   Package versions

-   Glossary of terms

### Global results

This page offers a summary of the results, grouped by test.

Details of the table:

-   File: Name of the file that contains the test. This column is
    clickable and will open the corresponding page.

-   Test: Label given to the test.

-   Expectations: Number of expectations contained in the test.

-   Result: If the test contains one or more error(s), failed
    expectation(s), or warning(s) ❌, otherwise ✅.

-   Time spent: Time spent on this particular test.

### Test-files.R

Each test file has its own chapter, detailing the results of the
expectations.

The summary at the top gives an overview of the results for the given
test file.

Then, details are included about the tested file:

-   Test: Label of the test (i.e. the text in
    `test_that("label of the test", ...)`)

-   Description: The description of the test, as detailed in the
    `#' @description` roxygen tag used just before the expectation. If
    no description was set, it will be blank.

-   Expectation: Raw name of the expectation, *i.e* the R code used.

-   Result: Success / Failure / Warning / Errored / Skip / Was skipped
    (see “Expectations status” at the top of this page).

-   Location: Name of the file containing the test, with the line number
    where the expectation is located (for example,
    `test-hello-world.R#9` means the expectation started at line 9, in
    the `test-hello-world.R` file)

-   Test\_time: When was the test run.

### Aggregated

Aggregation of expectations that returned a failure/error, warning, or
were skipped.

-   Location: Name of the file containing the test, with the line number
    where the expectation is located (for example,
    `test-hello-world.R#9` means the expectation started at line 9, in
    the `test-hello-world.R` file)

-   Test: Label of the test (i.e. the text in
    `test_that("label of the test", ...)`)

-   Description: The description of the test, as detailed in the
    `#' @description` roxygen tag used just before the expectation. If
    no description was set, it will be blank.

-   Expectation: Raw name of the expectation, *i.e* the R code used.

-   Message: Message output by R when the expectation
    failed/warned/skip.

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
