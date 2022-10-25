#' Get the historical data export for for a specific company within a country.
#'
#' @param country_code Two digit country code. Ex: NL, DE, DK, SE, FI etc.
#' @param company_code The 21 digit eic code of the company as found on the API page.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#' @param max_pages Maximum pages. Defaults to 5000 to get all pages.
#'
#' @export
#'
#' @examples
#'
#' library(gie)
#'
#' tail(gie_gas_company_eic(country_code = "DE", company_code = "21X000000001160J"))
#'
#'
gie_gas_company_eic <- function(country_code, company_code, api_key = NULL, max_pages = 5000){

  area <- toupper(country_code)

  if(length(country_code) > 1){
    stop("country_code only accepts a vector of length one.")
  }


  cont_df <- gie_pagination_api(hostname = "agsi.gie.eu", country = country_code,
                            company = company_code,
                            max_pages = max_pages, api_key = api_key)


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
  cont_df <- dplyr::arrange(cont_df, gasDayStart)

  cont_df
}

#' Get the historical data export for for a specific company within a country.
#'
#' @param country_code Two digit country code. Ex: NL, DE, DK, SE, FI etc.
#' @param company_code The 21 digit eic code of the company as found on the API page.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#' @param max_pages Maximum pages. Defaults to 5000 to get all pages.
#'
#' @export
#'
#' @examples
#'
#' library(gie)
#'
#' eic <- gie_lng_company_eic(country_code = "DE",
#'                            company_code = "21X000000001368W")
#'
#'
gie_lng_company_eic <- function(country_code, company_code, api_key = NULL, max_pages = 5000){

  area <- toupper(country_code)

  if(length(country_code) > 1){
    stop("country_code only accepts a vector of length one.")
  }


  cont_df <- gie_pagination_api(hostname = "alsi.gie.eu", country = country_code,
                                company = company_code,
                                max_pages = max_pages, api_key = api_key)


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
  cont_df <- dplyr::arrange(cont_df, gasDayStart)

  cont_df
}

#' Get the historical data export for for a specific facility from a company within a country.
#'
#' @param country_code Two digit country code. Ex: NL, DE, DK, SE, FI etc.
#' @param company_code The 21 digit eic code of the company as found on the API page.
#' @param facility_code The 21 digit eic code of the facility as found on the API page.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#' @param max_pages Maximum pages. Defaults to 5000 to get all pages.
#'
#' @export
#'
#' @examples
#'
#' library(gie)
#'
#' tail(gie_gas_facility_eic(country_code = "DE", company_code = "21X000000001160J"
#'                           facility_code = "21Z000000000271O"))
#'
#'
gie_gas_facility_eic <- function(country_code, company_code, facility_code, api_key = NULL, max_pages = 5000){

  area <- toupper(country_code)

  if(length(country_code) > 1){
    stop("country_code only accepts a vector of length one.")
  }


  cont_df <- gie_pagination_api(hostname = "agsi.gie.eu", country = country_code,
                                company = company_code,
                                max_pages = max_pages, api_key = api_key)


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
  cont_df <- dplyr::arrange(cont_df, gasDayStart)

  cont_df
}

#' Get the historical data export for for a specific facility from a company within a country.
#'
#' @param country_code Two digit country code. Ex: NL, DE, DK, SE, FI etc.
#' @param company_code The 21 digit eic code of the company as found on the API page.
#' @param facility_code The 21 digit eic code of the facility as found on the API page.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#' @param max_pages Maximum pages. Defaults to 5000 to get all pages.
#'
#' @export
#'
#' @examples
#'
#' library(gie)
#'
#' eic <- gie_lng_facility_eic(country_code = "DE",
#'                            company_code = "21X000000001368W"
#'                            facility_code = "")
#'
#'
gie_lng_facility_eic <- function(country_code, company_code, facility_code,
                                 api_key = NULL, max_pages = 5000){

  area <- toupper(country_code)

  if(length(country_code) > 1){
    stop("country_code only accepts a vector of length one.")
  }


  cont_df <- gie_pagination_api(hostname = "alsi.gie.eu", country = country_code,
                                company = company_code, facility = facility_code,
                                max_pages = max_pages, api_key = api_key)


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
  cont_df <- dplyr::arrange(cont_df, gasDayStart)

  cont_df
}

