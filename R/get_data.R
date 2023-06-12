#' Download a dataset from the API INE service
#' @param dataset \code{string}. The possible values are "ene", "epf_personas", "epf_gastos", "enusc" or "esi"
#' @param version \code{string} by default is the newest version
#' @import glue
#' @import assertthat
#' @import xml2
#' @export
#' @return \code{dataframe}


get_data <- function(dataset, version = NULL, col_list = NULL) {

  match.arg(dataset, c("ene", "epf_personas", "epf_gastos", "enusc", "esi"))

  # Validate version
  if (!is.null(version) & validate_version(dataset, version) == FALSE) {
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

  data <- httr::GET(paste0(ip, glue::glue("/data/{dataset}/{version}") ))

  # Convert to dataframe
  json <- httr::content(data)

  df <- purrr::map(json$data, function(x) {unname(unlist(x)) } ) %>%
    dplyr::bind_cols()

  # Get columns to sort in the right order
  columns <- get_columns( dataset, version)
  if(!is.null(col_list)) {

    if(is_empty(setdiff(col_list, columns))){
      rlang::inform(c("v" = 'Fue posible seleccionar todas las columnas ingresadas'))
    } else if(setdiff(col_list, columns)  %>% identical(col_list)){

      rlang::abort(c('x' = glue('Ninguna de las columnas ingresadas existe en
                                los datos de {dataset} {version}')))

    } else{

      variables_faltantes = setdiff(col_list, columns) %>%
        paste0(collapse = ', ' )
      variables_disponibles = setdiff(col_list, variables_faltantes) %>%
        paste0(collapse = ', ' )

      rlang::warn(c('i' = 'No todas las variables ingresadas están disponibles',
        'v' = glue('Variables disponibles: {variables_disponibles}'),
                      'x' = glue('Variables faltantes: {variables_faltantes}'),
        'Se procederá a cargar las que sí están disponibles'))

      }

    columns = columns[columns %in% col_list]

  }
  df <- df[columns]

  return(df)
}

#' Get multiple datasets
#' @param dataset \code{string}
#' @param from \code{string} specific version of any survey
#' @param to \code{string} specific version of any survey
#' @param versions \code{character vector} with specific versions of any survey
#' @import purrr
#' @export
#' @return \code{list} containing all the datasets between the from and to parameters
#'
#'


get_many_data <- function(dataset, from = NULL, to = NULL, versions = NULL,
                          col_list = NULL) {

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

  # Download data
  datasets <-  purrr::map(versions, ~ get_data(dataset, .x, col_list = col_list) )

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

