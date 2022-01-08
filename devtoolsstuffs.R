usethis::use_build_ignore("devtoolsstuffs.R")
usethis::use_build_ignore("readmefigs/")
usethis::use_build_ignore(".gitattributes")
usethis::use_build_ignore("reference")

library(usethis)
library(desc)
library(glue)
library(attempt)
library(devtools)
library(testthat)

# Remove default DESC
unlink("DESCRIPTION")
# Create and clean desc
my_desc <- description$new("!new")
my_desc$set("Package", "testdown")
my_desc$set("Authors@R", "person('Colin', 'Fay', email = 'contact@colinfay.me', role = c('cre', 'aut'), comment = c(ORCID = '0000-0001-7343-1846') )")
my_desc$del("Maintainer")
my_desc$set_version("0.0.0.9000")
my_desc$set(Title = "'testhat' Results to Bookdown")
# The description of your package
my_desc$set(Description = "Turn your 'testthat' results a Bookdown.")

# The urls
my_desc$set("URL", "https://github.com/thinkr-open/testdown")
my_desc$set("BugReports", "https://github.com/thinkr-open/testdown/issues")
# Save everyting
my_desc$write(file = "DESCRIPTION")

# If you want to use the MIT licence, code of conduct, and lifecycle badge
usethis::use_mit_license(name = "ThinkR")
use_readme_rmd()
use_code_of_conduct()
use_lifecycle_badge("Experimental")
use_news_md()

# Documentation
usethis::use_vignette("try-testdown")


# Test that
usethis::use_testthat()
usethis::use_test("test_down")

# CI
usethis::use_github_action_check_standard()

# Get the dependencies ----
use_tidy_description()

attachment::att_amend_desc(extra.suggests = "bookdown")

# Create the example
pkgload::load_all()
unlink("docs")
test_down_example(book_path = "docs")
