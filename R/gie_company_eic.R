#' Get the historical data export for for a specific facility from a company within a country.
#'
#' @param country_code Two digit country code. Ex: NL, DE, DK, SE, FI etc.
#' @param eic_code The 21 digit eic code of the facility as found on the API page.
#' @param eic_company_code The 21 digit eic code of the company as found on the API page.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#' @param ... Forwarded to [gie_internal_page_request()]
#'
#' @export
#'
#'
gie_gas_company_eic <- function(country_code, eic_code, eic_company_code, api_key = NULL, ...){

  area <- toupper(country_code)

  if(length(country_code) > 1){
    stop("country_code only accepts a vector of length one.")
  }


  url <- paste0("https://agsi.gie.eu/api/data/", eic_code, "/", country_code, "/", eic_company_code)

  cont_df <- gie_internal_page_request(url, api_key, ...)

  if(nrow(cont_df) == 0){
    stop("No data with these parameters.")
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

  cont_df
}


#' Get the historical data export for for a specific facility from a company within a country.
#'
#' @param country_code Two digit country code. Ex: NL, DE, DK, SE, FI etc.
#' @param eic_code The 21 digit eic code of the facility as found on the API page.
#' @param eic_company_code The 21 digit eic code of the company as found on the API page.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#'
#' @export
#'
#' @examples {
#'
#' library(gie)
#'
#' eic <- gie_lng_company_eic(country_code = "DE",
#'                            eic_code = "21W000000000100J",
#'                            eic_company_code = "21X000000001368W")
#'
#' }
#'
gie_lng_company_eic <- function(country_code, eic_code, eic_company_code, api_key = NULL){

  area <- toupper(country_code)

  if(length(country_code) > 1){
    stop("country_code only accepts a vector of length one.")
  }

  if(is.null(api_key)){
    api_key <- Sys.getenv("GIE_PAT")
  }

  url <- paste0("https://alsi.gie.eu/api/data/", eic_code, "/", country_code, "/", eic_company_code)

  resp <- httr::GET(url = url,
                    httr::add_headers("x-key" = api_key))

  if(httr::status_code(resp) != 200){
    status_httr <- httr::http_status(resp)
    stop(paste("Category:", status_httr$category,
               "Reason:", status_httr$reason,
               "Message:", status_httr$message))
  }

  cont <- httr::content(resp, as = "text", encoding = "UTF-8")

  cont_df <- jsonlite::fromJSON(cont)

  if(length(cont_df) == 0){
    stop("No data with these parameters.")
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
  cont_df
}
