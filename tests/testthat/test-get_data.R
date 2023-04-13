
############
# get_data #
############

test_that("get_data throws right error", {
  expect_error(get_data("enusc", 2020) )
})


test_that("get_data returns a data.frame", {
  expect_equal(sum(class(get_data("enusc", "2020")) == "data.frame"), 1  )
})


data <- "enusc"

test_that("get_data returns right error when version doesn't exist", {
  expect_error(get_data(data, "2000"), get_available_versions(data) )
})

#################
# get_many_data #
#################


test_that("get_many_data returns the right number of datasets between two points", {
  expect_equal(length(get_many_data("ene",  from = "2021-12-nde", to = "2022-05-amj")), 6  )
})

test_that("get_many_data returns all the datasets between two points", {
  expect_equal(names(get_many_data("ene",  from = "2021-12-nde", to = "2022-05-amj")),
               c("2022-05-amj", "2022-04-mam", "2022-03-fma", "2022-02-efm", "2022-01-def", "2021-12-nde"))
})

# ERRORS
test_that("get_many_data is throwing an error when user inputs an invalid version in from parameter" , {
  expect_error(get_many_data("ene", from = "blabla", "pata"), "blabla version is not available")
})

test_that("get_many_data is throwing an error when user inputs an invalid version" , {
  expect_error(get_many_data("ene", to = "pata"), "you have to include from and to")
})

test_that("get_many_data is throwing an error when user inputs an invalid version" , {
  expect_error(get_many_data("ene", from = "no_existe", version = c("no_existe")), "you have to include from and to")
})


test_that("get_many_data is throwing an error when user inputs an invalid version" , {
  expect_error(get_many_data("ene", version = c("no_existe")), "Some of the versions in your vector are invalid")
})





