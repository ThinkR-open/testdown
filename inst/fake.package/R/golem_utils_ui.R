#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @importFrom htmltools tags HTML
#'
#' @return an html element
#'
#' @export
#'
#' @examples
#' with_red_star("Enter your name here")
#'

with_red_star <- function(text) {
  htmltools::tags$span(
    HTML(
      paste0(
        text,
        htmltools::tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}

#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @importFrom htmltools tags tagAppendAttributes tagList
#'
#' @export
#'
#' @examples
#' list_to_li(c("a","b"))

list_to_li <- function(list, class = NULL){
  if (is.null(class)){
    tagList(lapply(list, tags$li))
  } else {
    res <- lapply(list, tags$li)
    res <- lapply(res, function(x) tagAppendAttributes(x, class = class))
    tagList(res)
  }

}
