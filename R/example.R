#' Run a test_down example
#'
#' @inheritParams test_down
#'
#' @return Path to the report
#' @export
#'
#' @examples
#' if (interactive()){
#'    test_down_example()
#' }
test_down_example <- function(
  project_name = NULL,
  author = NULL,
  book_path = "tests/testdown",
  with_help = TRUE,
  open = interactive()
){
  test_down(
    project_name = project_name,
    author = author,
    pkg = system.file("fakepkg", package = "testdown"),
    book_path = book_path,
    with_help = with_help,
    open = open
  )
}
