#' Download a dataset from the API INE service
#' @param dataset \code{string}. The possible values are "ene", "epf_personas", "epf_gastos", "enusc" or "esi"
#' @param version \code{string} by default is the newest version
#' @import glue
#' @import assertthat
#' @import xml2
#' @export
#' @return \code{dataframe}

get_data <- function(dataset, version = NULL) {

  # Validate arguments
  assertthat::assert_that(is.character(version))
  match.arg(dataset, c("ene", "epf_personas", "epf_gastos", "enusc", "esi"))

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

  data <- httr::GET(paste0(ip, glue::glue("/data/{dataset}/{version}") ))

  # Convert to dataframe
  json <- httr::content(data)

  df <- purrr::map(json$data, function(x) {unname(unlist(x)) } ) %>%
    dplyr::bind_cols()

  # Get columns to sort in the right order
  columns <- get_columns( dataset, version)
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
get_many_data <- function(dataset, from = NULL, to = NULL, versions = NULL) {

  match.arg(dataset, c("ene", "epf_personas", "epf_gastos", "enusc", "esi"))

  # from = "vii"
  # to = "2022-05-amj"

  # Validate relation between from-to and versions
  if ( (is.null(from) & is.null(to) & is.null(versions)  )) {
    stop("you have to select from-to or versions")
  }  else if ( (!is.null(from) & is.null(to) ) | (is.null(from) & !is.null(to)) ) {
    stop("you have to include from and to")
  } else if ((!is.null(from) & is.null(to) &  !is.null(versions))) {
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
    # If there is one or more invalid versions, an error is thrown
    if (sum(map_lgl(versions, ~validate_version(dataset, .x))) != length(versions)  ) {
      stop("Some of the versions in your vector are invalid")
    }
  }


  # Create list of versions. If the parameters "from and "to" are selected we filter all the versions between tthe 2 dates.
  if (is.null(versions)) {
    versions <- get_catalog(dataset) %>%
      dplyr::filter(version >= from & version <= to) %>%
      dplyr::pull(version)
  }

  # Download data
  datasets <-  purrr::map(versions, ~ get_data(dataset, .x) )
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

