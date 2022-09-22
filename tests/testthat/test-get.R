context("test-get")



test_that("Catalog must be dataframe", {
  expect_equal(sum(class(get_catalog()) == "data.frame"), 1)
})
