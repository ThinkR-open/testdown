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

build_were_skipped <- function(
  were_skipped
){
  if (length(were_skipped)){

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
  } else {
    were_skipped_df <- data.frame(
      row.names = NULL,
      stringsAsFactors = FALSE,
      context = character(0),
      test = character(0),
      expectation = character(0),
      description = character(0),
      location = character(0),
      test_time = character(0),
      result  = character(0),
      file =  character(0),
      message = character(0)
    )
  }
  were_skipped_df
}

#' @importFrom roxygen2 parse_text
#' @importFrom purrr map flatten keep
get_desc <- function(
  result
){
  fls <- readLines(normalizePath(attr(result$srcref, "srcfile")$filename))
  parsed <- parse_text(fls, NULL)
  tags <- map(parsed, "tags") %>% flatten()
  expectation_line <- result$srcref[1]

  # Scope the search to the current test_that() block so a description
  # belonging to a previous test_that doesn't bleed into this one — found
  # while reviewing my own #18 fix. The block starts at the most recent
  # line at or above `expectation_line` that opens a test_that() call.
  block_start <- find_test_that_start(fls, expectation_line)

  # Pick the closest @description tag *above* this expectation, but
  # within the same test_that block. Previously the code only looked
  # exactly 1 or 2 lines above, which missed descriptions placed further
  # up (e.g. before intermediate setup calls inside the same block).
  tag_lines <- vapply(tags, function(x) x$line %||% NA_integer_, integer(1))
  candidates_idx <- which(
    !is.na(tag_lines) &
      tag_lines < expectation_line &
      tag_lines >= block_start
  )
  if (length(candidates_idx)) {
    nearest <- candidates_idx[which.max(tag_lines[candidates_idx])]
    val <- tags[[nearest]]$val
    # A multi-line description would otherwise carry a literal newline
    # into the kable cell and spill the trailing portion onto the wrong
    # column (#18). Collapse any inner newlines into a single space.
    gsub("[[:space:]]*\n[[:space:]]*", " ", val)
  } else {
    " "
  }
}

#' Find the line number of the most recent `test_that(` call at or above
#' `line` in `fls`. Returns `1L` (start-of-file) when none is found, which
#' lets `get_desc()` fall back to file-scope behaviour.
#'
#' @param fls character vector, the file's lines.
#' @param line integer, current expectation line.
#' @noRd
find_test_that_start <- function(fls, line) {
  if (line < 1L || !length(fls)) return(1L)
  upper <- min(line, length(fls))
  for (i in upper:1L) {
    if (grepl("\\btest_that\\s*\\(", fls[i])) return(i)
  }
  1L
}

# tiny null-coalescer for older R / safer reads
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Sanitize a string for inclusion in a kable cell.
#'
#' kable interprets a double newline as a paragraph break, which spills the
#' tail of the cell onto the next row of the rendered table (#4). Collapse
#' consecutive blank lines to a single newline before passing the value
#' through.
#'
#' @param x character vector.
#' @return character vector, same length, safe to drop into a kable cell.
#' @noRd
sanitize_kable_cell <- function(x) {
  if (!length(x)) return(x)
  out <- gsub("\r\n", "\n", x, fixed = TRUE)
  # Collapse runs of blank lines into a single newline.
  out <- gsub("(\n[[:blank:]]*){2,}", "\n", out)
  out
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
