# No Remotes ----
# Attachments ----
to_install <- c("attempt", "bookdown", "devtools", "dplyr", "glue", "knitr", "R6", "rmarkdown", "stats", "testthat", "tidyr", "utils")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }

  }