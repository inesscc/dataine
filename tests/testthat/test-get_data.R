
############
# get_data #
############

test_that("get_data throws right error", {
  expect_error(get_data("enusc", 2020) )
})


test_that("get_data returns a data.frame", {
  expect_equal(sum(class(get_data("enusc", "2020",col_list = c("rph_edad","rph_sexo"))) == "data.frame"), 1  )
})


data <- "enusc"

test_that("get_data returns right error when version doesn't exist", {
  expect_error(get_data(data, "2000"), get_available_versions(data) )
})

#### get data no save localy ####
enusc = get_data("enusc", "2020",col_list = c("rph_edad","rph_sexo"))

test_that("testing that data is not saved locally",{
expect_equal(length(list.files(path = "data/", pattern = "enusc_2017.rds")),0)
})

rm(enusc)

#### save local ####

unlink("data/",recursive = T,force = T)

jue =  get_many_data("ene",  from = "2010-02-efm", to = "2010-03-fma", save_where = "disk", col_list = c("sexo","parentesco","curso"))



# jue = get_many_data("ene",  from = "2010-02-efm", to = "2010-03-fma", save_where = "disk", dont_ask_me = T,memory_warning_limit = 10)


test_that("Expect to create a file in working directory",{
expect_equal(list.files(path = "data/", pattern = "ene_2010-02-efm.rds"),"ene_2010-02-efm.rds")
})

unlink("data", force = T, recursive = T)

#### save both ####

unlink("data", force = T, recursive = T)



datos = get_many_data("ene",
                      from = "2010-02-efm",
                      to = "2010-03-fma",
                      save_where = "both",
                      memory_warning_limit = 10,
                      col_list = c("sexo","parentesco","curso"))

test_that("Expect to create a file in working directory",{
  expect_equal(list.files(path = "data/", pattern = "ene_2010-02-efm.rds"),"ene_2010-02-efm.rds")
})


test_that("Expect object with in R enviroment",{
  expect_equal(class(datos[[1]]),"data.frame")
})

rm(datos)
unlink("data", force = T, recursive = T)


### Manualy prompt testing save local with many data ####

### no message

# datos = get_many_data("ene",  from = "2010-02-efm", to = "2010-03-fma", save_where = "no_message",memory_warning_limit = 10, col_list = c("sexo","parentesco","curso"))

# l_ene <- get_many_data(dataset = "ene",from = "2010-02-efm", to = "2010-05-amj")
#
## in local
# l_ene <- get_many_data(dataset = "ene",from = "2010-02-efm", to = "2010-03-fma",save_where = "disk")
#
#### testing resume download all again with prompt
# l_ene <- get_many_data(dataset = "ene",from = "2010-02-efm", to = "2010-03-fma",save_where = "disk")
#
# ### testing resume download all again with some new files
# l_ene <- get_many_data(dataset = "ene",from = "2010-02-efm", to = "2010-05-amj",save_where = "disk")
#
## l_ene <- get_many_data(dataset = "ene",from = "2010-02-efm", to = "2010-05-amj",save_where = "both")


### testing save local with many data ####

test_that("get_many_data returns the right number of datasets between two points, setting 'save_where' == 'no_warning', for avoid prompt", {
  expect_equal(length(get_many_data("ene",
                                    from = "2010-02-efm",
                                    to = "2010-05-amj",
                                    save_where = "no",
                                    col_list = c("mes_central")
                                    )
                      ), 4  )
})


#get_many_data("ene", versions = c("2010-02-efm","2010-03-fma","2010-04-mam"))



### testing internal resume download ####

# get_many_data(dataset = "ene",from = "2010-02-efm", to = "2010-03-fma",save_where = "disk", col_list = c("sexo","parentesco","curso"))
#
# dataset = "ene"
# version = c("2010-02-efm","2010-03-fma","2010-04-mam","2010-05-amj")
#
# resume_download(dataset,version)


########################
# get specific columns #
########################

### testing internar function verify_columns

## test All inputted columns were selected
col_list = c("rph_ID","enc_idr","enc_region","enc_provincia","enc_comuna")

verify_columns("enusc","2017",col_list)

test_that("Expect right info message",{
expect_message(verify_columns("enusc","2017",col_list),"All inputted columns were selected")
})


## test warning get only some columns

col_list = c("rph_ID","enc_idr","enc_region","enc_provincia","enc_comuna30")

test_that("Expect right warning message",{
  expect_warning(verify_columns("enusc","2017",col_list))
})


## test no columns where get
test_that("Expect right warning message",{
  expect_error(verify_columns("enusc","2017",c("bla","ble")))
})



#################
# get_many_data #
#################

# "2010-02-efm","2010-03-fma","2010-04-mam","2010-05-amj"

test_that("get_many_data returns the right number of datasets between two points", {
  expect_equal(length(get_many_data("ene",  from = "2010-02-efm", to = "2010-07-jja",col_list = c("sexo","parentesco","curso"))), 6  )
})


test_that("get_many_data returns all the datasets between two points", {
  expect_equal(names(get_many_data("ene",  from = "2010-02-efm", to = "2010-07-jja",col_list = c("sexo","parentesco","curso"))),
               rev(c("2010-02-efm", "2010-03-fma", "2010-04-mam", "2010-05-amj", "2010-06-mjj","2010-07-jja")))
})

# ERRORS
test_that("get_many_data is throwing an error when user inputs an invalid version in from parameter" , {
  expect_error(get_many_data("ene", from = "blabla", "pata"), "blabla version is not available")
})

test_that("get_many_data is throwing an error when user inputs an invalid version" , {
  expect_error(get_many_data("ene", to = "pata"), "you have to include from and to")
})

# test_that("get_many_data is throwing an error when user inputs an invalid version" , {
#   expect_error(get_many_data("ene", from = "no_existe", version = c("no_existe")), "you have to include from and to")
# })


# test_that("get_many_data is throwing an error when user inputs an invalid version" , {
#   expect_error(get_many_data("ene", version = c("no_existe")), "Some of the versions in your vector are invalid")
# })

enusc <- get_data("enusc","2017","P1_1_1")

test_that("get_data with one column", {
  expect_equal(names(enusc), c("P1_1_1"))
})




