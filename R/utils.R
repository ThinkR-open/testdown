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
