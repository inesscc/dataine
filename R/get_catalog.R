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
    request <- httr::GET(paste0(ip, "/datasets"))
  } else {
    request <- httr::GET(paste0(ip, "/datasets/", dataset))
  }

  # Extract content from server answer
  json <- httr::content(request)


  # Convert to dataframe
  full_catalog <- jsonlite::fromJSON(json, simplifyVector = T) %>%
    dplyr::arrange(desc(.data$survey)) %>%
    dplyr::ungroup()



  return(full_catalog)
}

