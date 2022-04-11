


#' Get the table with storage data for a specific date.
#'
#' @param date Iso date.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#'
#' @export
#'
#' @examples {
#'
#' library(gie)
#' library(tidyverse)
#'
#' df <- gie_gas_date(as.Date("2022-01-01"))
#'
gie_gas_date <- function(date = Sys.Date() - 1, api_key = NULL){

  if(is.null(api_key)){
    api_key <- Sys.getenv("GIE_PAT")
  }

  url <- paste0("https://agsi.gie.eu/api?date=", date)

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

  # clean master - area
  df_master <- cont_df$data
  df_master <- dplyr::select(df_master, -children, -info)
  df_master <- dplyr::rename(df_master, area_name = name, area_code = code)
  df_master <- dplyr::mutate(df_master,
                             country_name = as.character(NA), country_code = as.character(NA),
                             company_name = as.character(NA), company_code = as.character(NA),
                             storage_name = as.character(NA), storage_code = as.character(NA),
                             data_level = 0L)

  # clean child1 - country
  df_child1 <- cont_df$data
  df_child1 <- df_child1[, c("name", "code", "children")]
  df_child1$children <-
    lapply(df_child1$children,
                      function(x){
                        x <- dplyr::select(x, -info)
                        x
                      })
  df_child1 <- dplyr::rename(df_child1, area_name = name, area_code = code)
  df_child1 <- tidyr::unnest(df_child1, children)
  df_child1 <- dplyr::rename(df_child1, country_name = name, country_code = code)
  df_child1 <- dplyr::mutate(df_child1, company_name = as.character(NA), company_code = as.character(NA),
                             storage_name = as.character(NA), storage_code = as.character(NA),
                             data_level = 1L)

  # clean child2 - company
  df_child2 <- df_child1
  df_child2 <- df_child2[, c("area_name", "area_code", "country_name", "country_code", "children")]
  df_child2$children <-
    lapply(df_child2$children,
           function(x){
             x <- dplyr::select(x, -info)
             x
           })
  df_child2 <- tidyr::unnest(df_child2, children)
  df_child2 <- dplyr::rename(df_child2, company_code = code, company_name = name)
  df_child2 <- dplyr::mutate(df_child2, storage_name = as.character(NA), storage_code = as.character(NA),
                             data_level = 2L)

  df_child3 <- df_child2
  df_child3 <- df_child3[, c("area_name", "area_code", "country_name", "country_code", "company_name", "company_code", "children")]
  df_child3$children <-
    lapply(df_child3$children,
           function(x){
             x <- dplyr::select(x, -info)
             x
           })
  df_child3 <- tidyr::unnest(df_child3, children)
  df_child3 <- dplyr::rename(df_child3, storage_code = code, storage_name = name)
  df_child3 <- dplyr::mutate(df_child3, data_level = 3L)

  df_child1 <- dplyr::select(df_child1, -children)
  df_child2 <- dplyr::select(df_child2, -children)

  cont_df <- dplyr::bind_rows(df_child3, df_child2, df_child1, df_master)

  cont_df <- suppressMessages(readr::type_convert(cont_df, na = c("", "NA", "-")))

  cont_df
}



