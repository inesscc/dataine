#ip <- "http://143.198.79.143:5003"
# ip <- "http://127.0.0.1:8000"
#ip <- "http://192.168.1.4:2000"
ip <- "http://10.90.2.47:7000"
#ip <- "http://localhost:90"

#' Get columns of a specific dataset
#' @param dataset \code{string}. The possible values are "ene", "epf_personas", "epf_gastos", "enusc" or "esi"
#' @param version \code{string} indicating the id for a dataset. This value can be obtained through the get_catalog function
#' @export
#' @return \code{character vector}
#' @examples
#' # Get columns of "enusc" 2017 survey
#' get_columns(dataset = "enusc", version = "2017")
#'
get_columns <- function(dataset, version) {
# Chequeos:
  match.arg(dataset, c("ene", "epf_personas", "epf_gastos", "enusc", "esi"))
  assertthat::assert_that(is.character(version))

  # request

  response <- httr::GET(paste0(ip, glue("/colnames?dataset={dataset}&version={version}") ))



  if(response$status_code == 404){
    versiones_disponibles = get_catalog() %>% filter(survey == dataset) %>% pull(version)

    stop(glue('Version "{version}" does not exist for dataset "{dataset}"\nThe available versions are:\n{paste0(versiones_disponibles,
                        collapse = "\n")}'))


  } else{
    columns <-  httr::content(response) %>%
      unlist() %>%
      unname()

    columns <- columns[columns != "index"]
    return(columns)

  }

}
