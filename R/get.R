
ip <- "http://143.198.79.143:5000"

#' Get catalog of data available
#' @import httr
#' @return \code{dataframe}

get_catalog <- function() {

  # request
  datasets <- httr::GET(paste0(ip, "/datasets"))

  # Extract content
  json <- httr::content(datasets)

  # Convert to a dataframe
  purrr::map(json$datasets, unlist) %>%
    purrr::imap(~tibble::tibble(version = .x)) %>%
    purrr::imap(~dplyr::mutate(.x, encuesta = .y)) %>%
    dplyr::bind_rows() %>%
    dplyr::relocate(encuesta)

}

#' Get catalog of data available
#' @param dataset \code{string}
#' @param version \code{string}
#' @import glue
#' @return \code{dataframe}

get_data <- function(dataset, version) {

  # request
  data <- httr::GET(paste0(ip, glue("/data/{dataset}/{version}") ))

  # Convert to dataframe
  json <- httr::content(data)
  df <- purrr::map(json$data, function(x) {unname(unlist(x)) } ) %>%
    dplyr::bind_cols()

  return(df)
}


