#ip <- "http://localhost:90"
#' Download a dataset from the API INE service
#' @param dataset \code{string}. The possible values are "ene", "epf_personas", "epf_gastos", "enusc" or "esi".
#' @param version \code{string} by default is the newest version.
#' @param col_list \code{character vector} by default is all columns. The possible values can be obtained through the get_columns function.
#' @param save_where \code{string} by default is "renviron", the possible values are "renviron","disk","both","no_message".
#' @import glue
#' @import assertthat
#' @import xml2
#' @export
#' @return \code{dataframe}
#' @examples
#' # Get dataset of "ene" survey from september-november-october 2020 quarter and selecting columns
#'
#' get_data(dataset="ene", version="2022-10-son", col_list=c("region","cae_general","fact_cal"))
#'

get_data <- function(dataset, version = NULL, col_list = NULL, save_where = c("renviron","disk","both","no_message")) {

  save_where = match.arg(save_where)

  match.arg(dataset, c("ene", "epf_personas", "epf_gastos", "enusc", "esi"))

  # Check lenght of col_list
  if(length(col_list) == 1) {

    col_list <- list(col_list[1])

  }

  # Validate version
  if (!is.null(version) && validate_version(dataset, version) == FALSE) {
    stop(get_available_versions(dataset))
  }

  # Return newest version if the user doesn't provide none of them
  if (is.null(version)) {
    version <- get_catalog(dataset) %>%
      dplyr::slice(1) %>%
      dplyr::pull(version)
  }

  # Validate arguments
  #assertthat::assert_that(is.character(version)) # esto nunca va a

columns <- verify_columns(dataset,version,col_list)


if(is.null(col_list)){
  body <- list("dataset"=dataset,
               "version"=version)
}else{
  body <- list("dataset"=dataset,
               "version"=version,
               "col_list"=columns
               )
}

rlang::inform(glue("Downloading {dataset}_{version} please wait"))

cap_speed <- config(max_recv_speed_large = 10000)

  data <- httr::POST(url = paste0(ip,"/data/"),
                     progress(),
                     config = list(cap_speed),
                     encode = "json",
                     body = body)

rlang::inform(glue("Please wait..."))


  df = data$content %>% RcppSimdJson::fparse() %>% RcppSimdJson::fparse()

  # print advance

 # print(glue("download {i} from {n_version}"))

### if save local
  if(any(save_where %in% c("disk","both"))){

  save_data  =  data$content %>% RcppSimdJson::fparse() %>% RcppSimdJson::fparse()

  if(dir.exists("data/")==F){
    dir.create("data/")
  }

  saveRDS(save_data,glue("data/{dataset}_{version}.rds"))

  # actualizamos descarga
  upload_tracker_file(dataset,version)

  }

if(any(save_where %in% c("renviron","both","no_message"))){
  return(df)
}else{
  rlang::inform(glue("The dataset was save in folder 'data' as: '{dataset}_{version}.rds'"))

}

}

#' Get multiple datasets
#' @param dataset \code{string}
#' @param from \code{string} specific version of any survey
#' @param to \code{string} specific version of any survey
#' @param col_list \code{character vector} by default is all columns. The possible values can be obtained through the get_columns function
#' @param versions \code{character vector} with specific versions of any survey
#' @param save_where \code{string} by default is "renviron", the possible values are "renviron","disk","both", "no_message".
#' @param memory_warning_limit \code{numeric} by default is 900 mb, changes the data limit in warning megabytes to prevent excessive loading of the R environment memory
#' @import purrr
#' @export
#' @return \code{list} containing all the datasets between the from and to parameters
#' @examples
#'
#' # Get all datasets from efm and mam quaters of ene survey
#'
#' get_many_data(dataset="ene", from="2022-08-jas", to="2022-10-son", col_list=c("ano_trimestre","mes_central","fact_cal","ocup_form"))
#'


get_many_data <- function(dataset, from = NULL, to = NULL,col_list = NULL, save_where = c("renviron","disk","both", "no_message"), memory_warning_limit=900) {
  versions = NULL

  save_where <- match.arg(save_where)

  match.arg(dataset, c("ene", "epf_personas", "epf_gastos", "enusc", "esi"))


  # from = "vii"
  # to = "2022-05-amj"

  # Validate relation between from-to and versions
  if ( (is.null(from) & is.null(to) & is.null(versions)  )) {
    stop("you have to select from-to or versions")
  }  else if ( (!is.null(from) & is.null(to) ) | (is.null(from) & !is.null(to)) ) {
    stop("you have to include from and to")
  } else if ((!is.null(from) & !is.null(to) &  !is.null(versions))) {
    stop("You can't select from-to and versions at the same time")
  }


  # Validate versions included inside "from" and "to"
  if (!is.null(from) && validate_version(dataset, version = from) == FALSE ) {
    stop(paste0(from, " version is not available"))
  } else if (!is.null(to) && validate_version(dataset, version = to) == FALSE ) {
    stop(paste0(to, " version is not available"))
  }

  # Validate "versions" parameter
  if (!is.null(versions) ) {
    validacion_versiones = map_lgl(versions, ~validate_version(dataset, .x))
    # If there is one or more invalid versions, an error is thrown
    if (all(validacion_versiones) == T  ) {

      invalid_versions = versions[(1- validacion_versiones) %>% as.logical()]


      stop(glue("Some of the versions in your vector are invalid:
                {paste0(invalid_versions, collapse = ', ')}"))
    }
  }


  # Create list of versions. If the parameters "from and "to" are selected we filter all the versions between the 2 dates.
  if (is.null(versions)) {
    versions <- get_catalog(dataset) %>%
      dplyr::filter(version >= from & version <= to) %>%
      dplyr::pull(version)
  }

  if(save_where == "renviron"){
  give_version_size_warning(dataset,versions,memory_warning_limit)
  }

  if(save_where %in% c("disk","both")){

    if(dir.exists("data/")==F){
      dir.create("data/")
    }


    versions = resume_download(dataset,versions)
  }


  # Download data
  datasets <-  purrr::map(versions, ~ get_data(dataset, .x, col_list,save_where) )
  rlang::inform(glue("Download finish!!"))


  names(datasets) <- versions
  return(datasets)
}

###########
# HELPERS #
###########

# Validate version
validate_version <- function(dataset, version) {
  available_versions <- get_catalog(dataset) %>%
    dplyr::pull(version)
  return(version %in% available_versions)
}


get_available_versions <- function(dataset) {
  available_versions <- get_catalog(dataset) %>%
    dplyr::pull(version) %>%
    paste0( collapse = ", ")
  return(paste0("The available versions for your survey are ", available_versions) )

}

### Validate existing columns
verify_columns = function(dataset,version,col_list){

  columns <- get_columns( dataset, version)

  if(!is.null(col_list)) {

    if(is_empty(setdiff(col_list, columns))){
      rlang::inform(c("v" = 'All inputted columns were selected'))

      columns <- col_list

    } else if(setdiff(col_list, columns)  %>% identical(col_list)){

      rlang::abort(c('x' = glue('None of the inputted columns exist in {dataset} {version}')))

    } else{

      variables_faltantes = setdiff(col_list, columns) %>%
        paste0(collapse = ', ' )
      variables_disponibles = setdiff(col_list, variables_faltantes) %>%
        paste0(collapse = ', ' )

      rlang::warn(c('i' = 'Not all inputted columns are available',
                    'v' = glue('Available columns: {variables_disponibles}'),
                    'x' = glue('Missing columns: {variables_faltantes}'),
                    'Proceeding to load available columns.'))

      columns = columns[columns %in% setdiff(col_list, variables_faltantes)]

    }


  }
  columns
}

### size version warning
give_version_size_warning = function(dataset,versions,memory_warning_limit=900){

  recursos_necesarios = get_catalog(dataset) %>% filter(version %in% versions) %>%
    summarise(size = sum(file_size_MB)) %>% pull(size)*5.55#/1024**2


  if(recursos_necesarios>memory_warning_limit){ ## 900
    rlang::inform("This download may overload your memory, we strongly recommend saving this data in your working directory,
                  using the parameter 'save_where' == 'local'. Do you still want to proceed? \n 1: yes \n 2: No \n")

    fe = readline(prompt="Answer: ")

  if(!fe %in% c(1,2)){
    rlang::abort("incorrect answer, value 1 and 2 as valid")
  }

  if(fe==2){
    rlang::inform("Set your preference in the 'save_where' parameter")
    stop_quietly()
    }

    if(fe==1){
      rlang::inform("Starting the download into the R environment memory, please wait, it may take several minutes.")
    }

}

}

## quiet stoping function, whitout error
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}



## uploader tracker file

upload_tracker_file <- function(dataset,version){

  file_path = "data/tracker.txt"

  # Check previus file
  if(length(list.files(path = "data/",pattern = "tracker.txt"))==1){

    my_txt0 <- readLines(file_path)
    my_txt <- paste0(dataset,"_",version)
    writeLines(c(my_txt0,my_txt), file_path) #3

  }else{

    my_txt <- paste0(dataset,"_",version)
    writeLines(c(my_txt), file_path) #3

  }

}

## resume download ###

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


