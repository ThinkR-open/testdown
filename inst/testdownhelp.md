## How to read {testdown} reports {-}

### Glossary {-}

#### Scopes of tests {-}

There are three scopes when it comes to tests:

+ "Test files", which are the files included inside the `test/testthat/` folder, and which contain a testing infrastructure.
Each file contains one ore more test(s).

+ "Tests", which start with `test_that("",`.
Each test contains one or more expectation(s).

+ "Expectations", which (usually) start with `expect_` (note that `skip_` functions are also considered as expectations).
An expectation only contains one statement.
If you put a roxygen tag `@description` __just before your expectation__, it will be displayed in the testing results (otherwise the 'Description' column will be blank).

To sum up, Test files contains one or more test(s), which contain one or more expectation(s).

#### Expectations status {-}

+ &#9989; <font color='green'>Success</font>: Passed (the expectation is met).

+ &#10060; <font color='red'>Failure</font>: Failed (the expectation is not met).

+ &#9888;&#65039; <font color='orange'>Warning</font>: The expectation returned a warning.

+ &#10060; <font color='red'>Error (test stopped)</font>: the expectation has returned an error, and the current test was stopped (i.e further expectations in the current test will not be launched).

+ &#128260; <font color='blue'>Skip</font>: This expectation has validated a "skip" expectation for the current test.

+ &#128260; <font color='blue'>Was Skipped</font>: The expectation has been skipped and is reported as a 'skipped expectation', due to a `skip_if` or an error previously in the test. 
Note that when an expectation is skipped, the Description, and Test time are not retrieved.

### First page {-}

The index offers a summary of:

+ When the tests were run

+ The project
  + Name of the project
  + Environment the tests were run into (aka the directory)
  + Details about the test results

+ The package (Title, Version, and Description of the package)

+ The testing infrastructure
  + R version
  + Operating system
  + Locale
  + Package versions


### Global results {-}

This page offers a summary of the results, grouped by test.

Details of the table:

+ File: Name of the file that contains the test. This column is clickable and will open the corresponding page.

+ Test: Name given to the test.

+ Expectations: Number of expectations contained in the test.

+ Result: If the test contains at least one error or failed expectation &#10060;, otherwise &#9989;.

+ Was skipped: Whether this test contains a skipped part or not.

+ Time spent: Time spent on this particular test.

### Test-files.R {-}

Each test file has its own chapter, detailing the results of the expectations.

The summary at the top gives an overview of the results for the given test file.

Then, details are included about the tested file:

+ Test: Label of the test (i.e. the text in `test_that("label of the test", ...)`)

+ Description: The description of the test, as detailed in the `#' @description` roxygen tag used just before the expectation. If no description was set, it will be blank.

+ Expectation: Raw name of the expectation, _i.e_ the R code used.

+ Result: Success / Failure / Warning / Errored / Skip / Was skipped (see "Expectations status" at the top of this page).

+ Location: Name of the file containing the test, with the line number where the expectation is located (for example, `test-hello-world.R#9` means the expectation started at line 9, in the `test-hello-world.R` file)

+ Test_time: When was the test run.

### Aggregated {-}

Aggregation of expectations that returned a failure/error, warning, or were skipped.

+ Location: Name of the file containing the test, with the line number where the expectation is located (for example, `test-hello-world.R#9` means the expectation started at line 9, in the `test-hello-world.R` file)

+ Test: Label of the test (i.e. the text in `test_that("label of the test", ...)`)

+ Description: The description of the test, as detailed in the `#' @description` roxygen tag used just before the expectation. If no description was set, it will be blank.

+ Expectation: Raw name of the expectation, _i.e_ the R code used.


+ Message: Message output by R when the expectation failed/warned/skip.

