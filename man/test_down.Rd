% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tomarkdown.R
\name{test_down}
\alias{test_down}
\title{testthat to bookdown}
\usage{
test_down(
  project_name = NULL,
  author = NULL,
  pkg = here::here(),
  environment = here::here(),
  book_path = "tests/testdown",
  with_help = TRUE,
  open = interactive()
)
}
\arguments{
\item{project_name}{The name you want to give to the project. The default is `NULL`, which will be then be converted to `basename(here::here())`.}

\item{author}{The author of the test report. Default is set to `NULL`, then it will be skipped.}

\item{pkg}{The path to the package to document. Default is `here::here()`.}

\item{environment}{A name for the testing environment. Default is `here::here()`.}

\item{book_path}{The path to the bookdown output. Default is `"tests/testdown"`.}

\item{with_help}{Should the help appendix be added? Default is `TRUE`.}

\item{open}{Should the bookdown be opened once compiled? Default is `interactive()`.}
}
\description{
This function turns the results of testthat into a bookdown. Each chapter is a context. The first page gives a summary of all the tests.
}
