#' Get the aggregated historical data export for Europe or Non Europe
#'
#' @param area eu for Europe, ne for Non Europe.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#' @param max_pages How many pages to return in the paginated results. Defaults to 1000
#'     so all results are returned as default.
#'
#' @export
#'
#' @examples {
#'
#' library(gie)
#' library(tidyverse)
#'
#' ne <- gie_gas_aggregate("ne")
#' eu <- gie_gas_aggregate("eu")
#'
#' eu %>%
#'   mutate(yr = year(gasDayStartedOn),
#'          mnth = month(gasDayStartedOn),
#'          mday = mday(gasDayStartedOn)) %>%
#'   filter(mnth == 7) %>%
#'   group_by(yr) %>%
#'   filter(mday == max(mday)) %>%
#'   ungroup() %>%
#'   ggplot(., aes(gasDayStartedOn, gasInStorage)) +
#'   geom_col() +
#'   geom_point(aes(gasDayStartedOn, workingGasVolume)) +
#'   labs(title = "End of June Gas Storage")
#' }
#'
gie_gas_aggregate <- function(area, api_key = NULL, max_pages = 1000, ...){

  area <- toupper(area)
  country_code = area

  # if(!area %in% c("ne", "eu") & length(area) > 1){
  #   stop("Area only accepts 'eu' or 'ne' and not both.")
  # }

  url <- paste0("https://agsi.gie.eu/api?continent=", area, "&size=300")

  cont_df <- gie_internal_page_request(url, api_key, max_pages = max_pages, country_code)

  if(nrow(cont_df) == 0){
    stop("No data for this area.")
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


#' Get the aggregated historical data export for Europe or Non Europe
#'
#' @param area eu for Europe, ne for Non Europe.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#' @param max_pages How many pages to return in the paginated results. Defaults to 1000
#'     so all results are returned as default.
#'
#' @export
#'
#' @examples {
#'
#' library(gie)
#' library(tidyverse)
#'
#' ne <- gie_lng_aggregate("ne")
#' eu <- gie_lng_aggregate("eu")
#'
#' eu %>%
#'   mutate(yr = year(gasDayStartedOn),
#'          mnth = month(gasDayStartedOn),
#'          mday = mday(gasDayStartedOn)) %>%
#'   filter(mnth == 7) %>%
#'   group_by(yr) %>%
#'   filter(mday == max(mday)) %>%
#'   ungroup() %>%
#'   ggplot(., aes(gasDayStartedOn, gasInStorage)) +
#'   geom_col() +
#'   geom_point(aes(gasDayStartedOn, workingGasVolume)) +
#'   labs(title = "End of June Gas Storage")
#' }
#'
gie_lng_aggregate <- function(area, api_key = NULL, max_pages = 1000){

  area <- toupper(area)
  country_code = area

  if(!area %in% c("NE", "EU") & length(area) > 1){
    stop("Area only accepts 'eu' or 'ne' and not both.")
  }

  if(is.null(api_key)){
    api_key <- Sys.getenv("GIE_PAT")
  }

  url <- paste0("https://alsi.gie.eu/api?continent=", area, "&size=300")

  cont_df <- gie_internal_page_request_lng(url, api_key, max_pages = max_pages, country_code = country_code)

  if(nrow(cont_df) == 0){
    stop("No data for this area.")
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
