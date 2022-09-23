


test_that("get_data throws right error", {
  expect_error(get_data("enusc", 2020) )
})


test_that("get_data returns a data.frame", {
  expect_equal(sum(class(get_data("enusc", "2020")) == "data.frame"), 1  )
})

test_that("get_data returns 0 rows when version doesn't exist", {
  expect_equal(nrow(get_data("enusc", "2000")), 0  )
})
