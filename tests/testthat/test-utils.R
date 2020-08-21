test_that("replace_in_file works", {
  tpf <- tempfile()
  write("hola hola hey", tpf)
  replace_in_file(
    tpf,
    "hola",
    "hello"
  )
  res <- readLines(
    tpf
  )
  #' @description The modified file should contain "hello"
  expect_true(
    grepl("hello", res)
  )
  #' @description The modified file should contain "hola"
  expect_false(
    grepl("hola", res)
  )
})
