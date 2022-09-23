
#ip <- "http://143.198.79.143:5000"
ip <- "http://127.0.0.1:8000"

#' Get catalog of available data
#' @param dataset \code{string} name of dataset
#' @import httr
#' @import dplyr
#' @importFrom rlang .data
#' @export
#' @return \code{dataframe}

get_catalog <- function( dataset = c("full", "ene", "epf_personas", "enusc")) {

  # check if the dataset is correct
  dataset <- match.arg(dataset)

  # request. It depends on the filter provided by user
  if (dataset == "full") {
    request <- httr::GET(paste0(ip, "/datasets"))
  } else {
    request <- httr::GET(paste0(ip, "/datasets/", dataset))
  }

  # Extract content from server answer
  json <- httr::content(request)

  # Convert to a dataframe
  full_catalog <-  purrr::map(json$datasets, unlist) %>%
    purrr::imap(~tibble::tibble(version = .x)) %>%
    purrr::imap(~dplyr::mutate(.x, encuesta = .y)) %>%
    dplyr::bind_rows() %>%
    dplyr::relocate(.data$encuesta) %>%
    dplyr::arrange(desc(version))

  return(full_catalog)
}


#' Get dataset
#' @param dataset \code{string}
#' @param version \code{string}
#' @import glue
#' @export
#' @return \code{dataframe}

get_data <- function(dataset, version = NULL) {

  # Return newest version if the user doesn't provide anyone
  if (is.null(version)) {
    version <- get_catalog(dataset) %>%
      dplyr::slice(1) %>%
      dplyr::pull(version)
  }
  # request
  data <- httr::GET(paste0(ip, glue::glue("/data/{dataset}/{version}") ))

  # Convert to dataframe
  json <- httr::content(data)
  df <- purrr::map(json$data, function(x) {unname(unlist(x)) } ) %>%
    dplyr::bind_cols()

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



#' Get columns of a dataset
#' @param dataset \code{string}
#' @param version \code{string}
#' @export
#' @return \code{list}
#'
get_columns <- function(dataset, version) {
  # request
  response <- httr::GET(paste0(ip, glue("/colnames/{dataset}/{version}") ))
  columns <-  httr::content(response) %>%
    unlist() %>%
    unname()
  return(columns)
}
