#' Get the aggregated historical data export for for a specific country.
#'
#' @param country_code Two digit country code. Ex: NL, DE, DK, SE, FI etc.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#'
#' @export
#'
#' @examples {
#'
#' library(gie)
#'
#' de <- gie_country("DE")
#' nl <- gie_country("NL")
#'
#' }
#'
gie_gas_country <- function(country_code, api_key = NULL, ...){

  country_code <- toupper(country_code)

  if(length(country_code) > 1){
    stop("country_code only accepts a vector of length one.")
  }


  url <- paste0("https://agsi.gie.eu/api?country=", country_code, "&size=300")



  cont_df <- gie_internal_page_request(url, api_key, max_pages = 5000, country_code = country_code)

  if(nrow(cont_df) == 0){
    stop("No data for this country.")
  }
  cont_df$info <- sapply(cont_df$info, function(x){
    if(length(x) < 1){
      x <- as.character(NA)
    } else {
      x <- paste(x, collapse = ";")
    }
    x
  })
  cont_df <- suppressMessages(readr::type_convert(cont_df, na = c("", "NA", "-")))
  cont_df <- dplyr::arrange(cont_df, gasDayStart)

  cont_df
}

#' Get the aggregated historical data export for for a specific country.
#'
#' @param country_code Two digit country code. Ex: NL, DE, DK, SE, FI etc.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#'
#' @export
#'
#' @examples {
#'
#' library(gie)
#'
#' de <- gie_lng_country("DE")
#' nl <- gie_lng_country("NL")
#'
#' }
#'
gie_lng_country <- function(country_code, api_key = NULL, max_pages = 5000){

  area <- toupper(country_code)

  if(length(country_code) > 1){
    stop("country_code only accepts a vector of length one.")
  }

  if(is.null(api_key)){
    api_key <- Sys.getenv("GIE_PAT")
  }

  url <- paste0("https://alsi.gie.eu/api?country=", country_code, "&size=300")

  cont_df <- gie_internal_page_request_lng(url, api_key, max_pages = max_pages, country_code = country_code)

  if(nrow(cont_df) == 0){
    stop("No data for this country.")
  }
  cont_df$info <- sapply(cont_df$info, function(x){
    if(length(x) < 1){
      x <- as.character(NA)
    } else {
      x <- paste(x, collapse = ";")
    }
    x
  })
  cont_df <- suppressMessages(readr::type_convert(cont_df, na = c("", "NA", "-")))
  cont_df <- dplyr::arrange(cont_df, gasDayStart)

  cont_df
}
