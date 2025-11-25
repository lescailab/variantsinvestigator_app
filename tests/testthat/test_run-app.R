# Load application support files into testing environment
# File: tests/testthat/test-app-function.R
library(shinytest2)

test_that("variantsinvestigator basic app test", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  app <- AppDriver$new(VariantsInvestigator(), name = "variantsinvestigator-app")

  app$expect_values()
})
