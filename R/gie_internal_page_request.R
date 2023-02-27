#' Get the historical data export for for a specific facility from a company within a country.
#'
#' @param url The base URL to request
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#' @param max_pages Page results if possible, up to this number of pages.
#'     Both a message and a warning are emitted if there are more pages
#'     than this parameter specifies.
gie_internal_page_request <- function(url, api_key, max_pages=10, country_code) {
  initial_url <- url

  if(is.null(api_key)){
    api_key <- Sys.getenv("GIE_PAT")
  }


  resp <- httr::RETRY('GET',
                      url = paste0(url, '?size=300'),
                      httr::add_headers("x-key" = api_key))

  if(httr::status_code(resp) != 200){
    status_httr <- httr::http_status(resp)
    stop(paste("Category:", status_httr$category,
               "Reason:", status_httr$reason,
               "Message:", status_httr$message))
  }

  cont <- httr::content(resp, as = "text", encoding = "UTF-8")

  cont_obj <- jsonlite::fromJSON(cont)

  cont_df <- cont_obj$data

  page <- 1

  if(cont_obj$last_page>max_pages) {
    w <- paste0(
        'Only returning ', max_pages, ' of ', cont_obj$last_page, ' pages. ',
        'Add max_pages parameter to load more data'
      )
    message(w)
    warning(w)
  }

  while(page<cont_obj$last_page && page<max_pages) {
    page <- page + 1

    url <- paste0(initial_url, "?size=300&page=", page)

    resp <- httr::RETRY('GET',
                        url = url,
                        httr::add_headers("x-key" = api_key))

    if(httr::status_code(resp) != 200){
      status_httr <- httr::http_status(resp)
      stop(paste("Category:", status_httr$category,
                 "Reason:", status_httr$reason,
                 "Message:", status_httr$message))
    }

    cont <- httr::content(resp, as = "text", encoding = "UTF-8")

    cont_obj <- jsonlite::fromJSON(cont)

    cont_df <- bind_rows(cont_df, cont_obj$data)

    #Sys.sleep(0.5)
  }

  cont_df
}


