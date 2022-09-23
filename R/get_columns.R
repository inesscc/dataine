
#ip <- "http://143.198.79.143:5000"
ip <- "http://127.0.0.1:8000"

#' Get columns of a dataset
#' @param dataset \code{string}
#' @param version \code{string}
#' @export
#' @return \code{list}
#'
get_columns <- function(dataset, version) {

  match.arg(dataset, c("ene", "epf_personas", "enusc"))

  # request
  response <- httr::GET(paste0(ip, glue("/colnames/{dataset}/{version}") ))
  columns <-  httr::content(response) %>%
    unlist() %>%
    unname()
  return(columns)
}
