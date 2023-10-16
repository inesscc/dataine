
############
# get_data #
############

test_that("get_data throws right error", {
  expect_error(get_data("enusc", 2020) )
})


test_that("get_data returns a data.frame", {
  expect_equal(sum(class(get_data("enusc", "2020", col_list = c("MODO_FINAL") )) == "data.frame"), 1  )
})


data <- "enusc"

test_that("get_data returns right error when version doesn't exist", {
  expect_error(get_data(data, "2000"), get_available_versions(data) )
})

#### get data no save localy ####
enusc = get_data("enusc", "2017", col_list = c("Score_Hogar"))

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

### no message



datos = get_many_data("ene",
                      from = "2010-02-efm",
                      to = "2010-03-fma",
                      save_where = "no_message",
                      memory_warning_limit = 10,
                      col_list = c("sexo","parentesco","curso"))

### Manualy prompt testing save local with many data ####

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

dataset = "ene"
version = c("2010-02-efm","2010-03-fma")#,"2010-04-mam","2010-05-amj")

if(dir.exists("data/")==F){
  dir.create("data/")
}

file_path = "data/tracker.txt"

my_txt0 <- readLines("data/tracker.txt")   #list.files("data/") #readLines(file_path)
my_txt <- paste0(dataset,"_",version)


old_vers = my_txt[my_txt %in% my_txt0]

new_vers = my_txt[!my_txt %in% my_txt0]

dsts = get_catalog() %>% pull(survey) %>% unique

dsts = paste0(dsts,"_",collapse = "|")

stringr::str_remove_all(my_txt,paste0(".rds|",dsts))

upload_tracker_file(dataset,version)


resume_download(data,version)

resume_download <- function(dataset,version){

  file_path = "data/tracker.txt"

  # Existing previus file

  if(length(list.files(path = "data/",pattern = "tracker.txt"))==1){

    my_txt0 <- readLines(file_path)
    my_txt <- paste0(dataset,"_",version)

    # 1 if all the requested versions are already tracked
    if(all(my_txt %in% my_txt0)){
      rlang::inform("According to the record (tracker.txt file), these versions have already been downloaded previously, do you want to download again?
                    \n 1: yes \n 2: No \n")

      fe = readline(prompt="Answer: ")

      if(!fe %in% c(1,2)){
        rlang::abort("incorrect answer, value 1 and 2 as valid")
      }


      if(fe==1){
        rlang::inform(c("Starting the download of: ",my_txt))
        file.remove("data/tracker.txt")
        # writeLines(c(my_txt), file_path) #3
        dsts = get_catalog() %>% pull(survey) %>% unique

        dsts = paste0(dsts,"_",collapse = "|")

        version = stringr::str_remove_all(my_txt,paste0(".rds|",dsts))

      }

      if(fe==2){
        rlang::inform("Canceling download")
        stop_quietly()
      }


      version
      # 2 if there are some old versions but some new
    }else if(any(my_txt %in% my_txt0) & length(my_txt[!my_txt %in% my_txt0])>=1){

      old_vers = my_txt[my_txt %in% my_txt0]

      new_vers = my_txt[!my_txt %in% my_txt0]

      rlang::inform(glue('\t According to the record (tracker.txt file), you have already downloaded the following versions:\n{paste0(old_vers,
                        collapse = "\n")},\n- And you have not yet downloaded the following versions:\n{paste0(new_vers,
                        collapse = "\n")} '))

      rlang::inform("\n Do you want to download all versions again or just the new ones?
                    \n 1: all \n 2: Just the new ones\n")

      fe = readline(prompt="Answer: ")

      if(!fe %in% c(1,2)){
        rlang::abort("incorrect answer, value 1 and 2 as valid")
      }


      if(fe==1){
        rlang::inform(c("Starting the download of all versions: ",my_txt))

        # writeLines(c(my_txt), file_path) #3
        file.remove("data/tracker.txt")

        dsts = get_catalog() %>% pull(survey) %>% unique

        dsts = paste0(dsts,"_",collapse = "|")

        version = stringr::str_remove_all(my_txt,paste0(".rds|",dsts))

      }

      if(fe==2){
        rlang::inform(c("Starting the download of the new versions: ",new_vers))
        #writeLines(c(my_txt), file_path) #3
        #writeLines(c(new_vers), file_path) #3
        dsts = get_catalog() %>% pull(survey) %>% unique

        dsts = paste0(dsts,"_",collapse = "|")

        version = stringr::str_remove_all(new_vers,paste0(".rds|",dsts))
      }

      version
      # 3 all version are new
    }else if(any(my_txt %in% my_txt0)==FALSE){

      dsts = get_catalog() %>% pull(survey) %>% unique

      dsts = paste0(dsts,"_",collapse = "|")

      version = stringr::str_remove_all(my_txt,paste0(".rds|",dsts))

      version
    }

  }else{
    ### create file
    # print("true")
    # my_txt <- paste0(dataset,"_",version)
    # writeLines(c(my_txt), file_path) #3

    version
  }

}


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


### get all columns ###
specific_cols = c("rph_ID","enc_idr","enc_region","enc_provincia","enc_comuna")

enusc <- get_data("enusc",version = "2017",col_list = get_columns("enusc", "2017")[1:5])

test_that(" get_data, returns specific cols 'col_list' parameter",{
  expect_equal(names(enusc),specific_cols)
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

enusc <- get_data("enusc","2017","P1_1_1")

test_that("get_data with one column", {
  expect_equal(names(enusc), c("P1_1_1"))
})




