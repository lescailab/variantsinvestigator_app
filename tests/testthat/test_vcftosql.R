# Load application support files into testing environment
# File: tests/testthat/test-app-function.R
library(shinytest2)

test_that("variantsinvestigator sql initial values are consistent", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  app <- AppDriver$new(vcfToSQL(), name = "variantsinvestigator-vcftosql")

  app$expect_values()
})
