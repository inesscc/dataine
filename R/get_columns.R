
ip <- "http://143.198.79.143:5003"
#ip <- "http://127.0.0.1:8000"

#' Get columns of a specific dataset
#' @param dataset \code{string}. The possible values are "ene", "epf_personas", "epf_gastos", "enusc" or "esi"
#' @param version \code{string} indicating the id for a dataset. This value can be obtained through the get_catalog function
#' @export
#' @return \code{character vector}
#'
get_columns <- function(dataset, version) {

  match.arg(dataset, c("ene", "epf_personas", "epf_gastos", "enusc", "esi"))

  # request
  response <- httr::GET(paste0(ip, glue("/colnames/{dataset}/{version}") ))
  columns <-  httr::content(response) %>%
    unlist() %>%
    unname()
  return(columns)
}
