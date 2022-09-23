


ene <- get_catalog("ene") %>%
  dplyr::pull(encuesta)

test_that("get_catalog works with ene dataset", {
  expect_equal(sum(ene == "ene"), length(ene) )
})


test_that("get_catalog throws right error", {
  expect_error(get_catalog("eanna") )
})
