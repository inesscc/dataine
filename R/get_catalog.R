#' Get catalog of available data
#' @param dataset \code{string} name of dataset. The possible values are "full", "ene", "epf_personas", "epf_gastos", "enusc" or "esi". The default value is "full"
#' @import httr
#' @import dplyr
#' @importFrom rlang .data
#' @export
#' @return \code{dataframe}

get_catalog <- function( dataset = c("full", "ene", "epf_personas", "epf_gastos", "enusc", "esi")) {

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


  # Convert to dataframe
  full_catalog <-  purrr::map(json, unlist) %>%
    purrr::imap(~tibble::tibble(version = .x)) %>%
    purrr::imap(~dplyr::mutate(.x, encuesta = .y)) %>%
    dplyr::bind_rows() %>%
    dplyr::relocate(.data$encuesta) %>%
    dplyr::group_by(.data$encuesta) %>%
    dplyr::arrange(desc(.data$version))



  return(full_catalog)
}
