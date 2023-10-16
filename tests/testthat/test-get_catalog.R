


ene <- get_catalog("ene") %>%
  dplyr::pull(survey)


test_that("get_catalog works with ene dataset", {
  expect_equal(sum(ene == "ene"), length(ene) )
})


test_that("get_catalog throws right error", {
  expect_error(get_catalog("eanna") )
})

# test reques with "full"

test_that("get_catalog request all available surveys",{
  expect_equal(unique(get_catalog()$survey),c("ene","enusc","epf_gastos","epf_personas","esi"))
})

