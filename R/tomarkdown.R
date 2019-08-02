expectation_type <- getFromNamespace("expectation_type", "testthat")

#' @importFrom testthat LocationReporter
#' @importFrom R6 R6Class

rmd_reporter <- R6Class(classname = "CR",
                            inherit = LocationReporter,
                            public = list(
                              ctx_list = c(),
                              start_test = function(context, test) {
                              },
                              initialize = function(...){
                                super$initialize(...)
                              },

                              add_result = function(context, test, result) {
                                ref <- result$srcref
                                if (is.null(ref)) {
                                  location <- "?#?:?"
                                } else {
                                  location <- paste0(
                                    basename(attr(ref, "srcfile")$filename), "#", ref[1], ":1")
                                }
                                temp_csv <- file.path(
                                  dirname(dirname(normalizePath(basename(attr(ref, "srcfile")$filename)))),
                                  "testdown", "testcsv.csv"
                                )
                                status <- expectation_type(result)
                                call <- paste(deparse(result$expectation_calls[[1]]), collapse = "")
                                call <- gsub(" {2,}", " ", call)
                                line_to_add <- paste0(context, "; `", call, "` ; ",
                                                      location, " ; ", Sys.time(), "; ", status,
                                                      ";", normalizePath(attr(ref, "srcfile")$filename) )
                                write(line_to_add, temp_csv, append = TRUE)
                              },

                              end_test = function(context, test) {
                                self$cat_line()
                                self$cat_line("Test ended at ", Sys.time())
                                self$cat_line()
                              }
                            )
)

#' testthat to bookdown
#'
#' This function turns the results of testthat into a bookdown. Each chapter is a context. The first page gives a summary of all the tests.
#'
#' @param pkg The path to the package. Default is `.`
#' @param book_path The path to the bookdown output. Default is `"tests/testdown"`.
#' @param open Should the bookdown be opened once compiled? Default is TRUE.
#'
#' @export
#'
#' @importFrom attempt if_not
#' @importFrom devtools as.package test
#' @importFrom dplyr group_by pull
#' @importFrom knitr kable knit
#' @importFrom rmarkdown render
#' @importFrom stats setNames
#' @importFrom tidyr nest
#' @importFrom utils read.csv2 browseURL data
#' @importFrom magrittr %>%
test_down <- function(pkg = ".", book_path = "tests/testdown", open = TRUE){
  meta <- as.package(pkg)
  unlink(file.path(pkg, book_path), recursive = TRUE)
  if_not(
    file.path(pkg, book_path),
    dir.exists,
    ~ dir.create(file.path(pkg, book_path),recursive = TRUE)
  )
  lapply(
    list.files(system.file("booktemplate/", package = "testdown"), full.names = TRUE),
    function(x){file.copy(from = x, to = normalizePath(file.path(pkg, book_path)))}
  )
  replace_in_file(
    file.path(pkg, book_path, "_bookdown.yml"),
    "teeest",
    meta$package)
  replace_in_file(
    file.path(pkg, book_path, "index.Rmd"),
    "XXXXXX",
    meta$package
  )
  replace_in_file(
    file.path(pkg, book_path, "index.Rmd"),
    "Yihui Xie",
    gsub("([^<]+) <.*", "\\1", eval(parse(text = meta$`authors@r`)))
  )
  temp_csv <- file.path(pkg, book_path, "testcsv.csv")
  file.create(temp_csv)
  on.exit(unlink(temp_csv))
  write(file = temp_csv, "Context; Test;Location;Test time;Result;File Name")
  a <- test(pkg, reporter = rmd_reporter)

  write_in <- function(x, there = file.path(pkg, book_path, "index.Rmd")){
    write(x, file = there, append = TRUE)
  }
  write_in("\n")
  write_in(paste("# Coverage results for package", meta$package,"{-} \n"))
  write_in(paste("Done on:", Sys.time(),"\n"))
  write_in("\n")
  write_in(kable(a))
  y <- read.csv2(temp_csv)
  x <- y %>%
    group_by(Context, File.Name) %>%
    nest()
  res <- pull(x, data)
  res <- setNames(res, x$Context)
  for (i in seq_along(res)){
    write_in("\n")
    write_in( paste( "#", names(res)[i] ) )
    write_in("\n")
    write_in(kable(res[i]))
    write_in("\n")
  }

  res <- render(
    file.path(pkg, book_path, "index.Rmd"))
  #knit(file.path(pkg, book_path, "index.Rmd"))
  if (open){
    browseURL(res)
  }
  res
}

#test_down(pkg = "../attempt/")

#' #' Testthat results to Rmd
#' #'
#' #' Turn your testthat results into a Rmd.
#' #'
#' #' @param pkg the path to your package
#' #' @param rmd_path the path where R will write the Rmd
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @importFrom devtools as.package test
#' #'
#' #' @examples
#'
#' #' @importFrom devtools as.package
#' test_to_rmd <- function(pkg = ".", rmd_path = "testdown.Rmd"){
#'   pkg <- as.package(pkg)
#'
#'   with_sink(rmd_path, {
#'     add_header(pkg)
#'     test(pkg, reporter = rmd_reporter)
#'
#'   })
#'
#' }
#'
#'
#'
#' #a <- test_down(pkg = "../attempt/", rmd_path = "res.Rmd")
#'
#' add_header <- function(pkg){
#'   print(glue("---"))
#'   print(glue('title: "Tests on package {pkg$package}"'))
#'   print(glue('subtitle: "Package version: {pkg$version}"'))
#'   print(glue("date: {Sys.Date()}"))
#'   print(glue("output:"))
#'   print(glue("  html_document:"))
#'   print(glue("    toc: true"))
#'   print(glue("    toc_float: true"))
#'   print(glue("---"))
#'
#' }
#'
#' clean_rmd <- function(rmd){
#'   res <- readLines(rmd)
#'   write(res[!grepl("^\\[", res)], rmd)
#' }
#'
#' # clean_rmd("res.Rmd")
#'
#' with_sink <- function(path, expr){
#'   sink(path, type = "output", append = TRUE)
#'   on.exit(sink())
#'   force(expr)
#' }
#'
#'
