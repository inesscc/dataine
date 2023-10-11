#' Get catalog of available data
#' @param dataset \code{string} name of dataset. The possible values are "full", "ene", "epf_personas", "epf_gastos", "enusc" or "esi". The default value is "full"
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @importFrom rlang .data
#' @export
#' @return \code{dataframe with two columns: survey and version}
#' 1. Columns
#'
#' \itemize{
#'   \item \code{survey} One of the surveys conducted by INE Chile
#'   \item \code{version} Specific version of a survey. It can be year or a quarter
#' }
#'


get_catalog <- function( dataset = c("full", "ene", "epf_personas", "epf_gastos", "enusc", "esi")) {

  # check if the dataset is correct
  dataset <- match.arg(dataset)

  # request. It depends on the filter provided by user
  if (dataset == "full") {
    request <- httr::GET(paste0(ip, "/info"))
  } else {
    request <- httr::GET(paste0(ip, "/info?dataset_filter=", dataset))
  }

  # Extract content from server answer
  json <- httr::content(request)


  # Convert to dataframe
  full_catalog <- jsonlite::fromJSON(json, simplifyVector = T) %>%
    dplyr::group_by(.data$survey) %>%
    dplyr::arrange(desc(.data$version),.by_group = T) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(file_size_MB = file_size/1000**2) %>%
    dplyr::select(-file_size)

  return(full_catalog)
}

