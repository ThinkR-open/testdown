#' Hello world!
#'
#' @param name A name
#'
#' @return A character vector with "Hello world, my name is name!"
#' @export
#'
#' @examples
#' hello_world("Colin")
hello_world <- function(name) {
  sprintf(
    "Hello world, my name is %s!",
    name
  )
}
