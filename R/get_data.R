#' Get dataset
#' @param dataset \code{string}
#' @param version \code{string} by default is the newest version
#' @import glue
#' @import assertthat
#' @import xml2
#' @export
#' @return \code{dataframe}

get_data <- function(dataset, version = NULL) {

  assertthat::assert_that(is.character(version))
  match.arg(dataset, c("ene", "epf_personas", "enusc"))

  # Return newest version if the user doesn't provide none of them
  if (is.null(version)) {
    version <- get_catalog(dataset) %>%
      dplyr::slice(1) %>%
      dplyr::pull(version)
  }
  # request
  # dataset <- "ene"
  # version <- "2022-07-jja"
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
#' @return \code{list}
#'
get_many_data <- function(dataset, from, to, versions = NULL) {

  match.arg(dataset, c("ene", "epf_personas", "enusc"))

  # Filter versions
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

