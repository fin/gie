#' Get the historical data export for for a specific facility from a company within a country.
#'
#' @param url The base URL to request
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#' @param max_pages Page results if possible, up to this number of pages.
#'     Both a message and a warning are emitted if there are more pages
#'     than this parameter specifies.
gie_internal_page_request <- function(url, api_key, max_pages=10, country_code) {

  if(is.null(api_key)){
    api_key <- Sys.getenv("GIE_PAT")
  }

  time_stamp <- Sys.time()

  resp <- httr::GET(url = url,
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
  # Requests will be qeued if more than 60 requests per second.
  request_time <- as.numeric(Sys.time() - time_stamp)
  if(request_time < 1){
    Sys.sleep(1.05 - request_time)
  }

  pages_to_get <- min(c(max_pages, cont_obj$last_page))

  # Initialise a progress bar
  pb <- txtProgressBar(min = 1, max = pages_to_get, style = 3)

  while(page<cont_obj$last_page && page<max_pages) {
    page <- page + 1

    time_stamp <- Sys.time()

    if(country_code %in% c("EU", "NE")){
      url <- paste0("https://agsi.gie.eu/api?continent=", country_code, "&size=300&page=", page)
    } else {
      url <- paste0("https://agsi.gie.eu/api?country=", country_code, "&size=300&page=", page)
    }

    resp <- httr::GET(url = url,
                      httr::add_headers("x-key" = api_key))

    if(httr::status_code(resp) != 200){
      status_httr <- httr::http_status(resp)
      stop(paste("Category:", status_httr$category,
                 "Reason:", status_httr$reason,
                 "Message:", status_httr$message))
    }

    cont <- httr::content(resp, as = "text", encoding = "UTF-8")

    cont_obj <- jsonlite::fromJSON(cont)

    cont_df <- dplyr::bind_rows(cont_df, cont_obj$data)

    # Requests will be qeued if more than 60 requests per second.
    request_time <- as.numeric(Sys.time() - time_stamp)
    if(request_time < 1){
      Sys.sleep(1.05 - request_time)
    }
    setTxtProgressBar(pb, page)
  }

  cont_df
}


#' Get the historical data export for for a specific facility from a company within a country.
#'
#' @param url The base URL to request
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#' @param max_pages Page results if possible, up to this number of pages.
#'     Both a message and a warning are emitted if there are more pages
#'     than this parameter specifies.
gie_internal_page_request_lng <- function(url, api_key, max_pages=10, country_code) {

  if(is.null(api_key)){
    api_key <- Sys.getenv("GIE_PAT")
  }

  time_stamp <- Sys.time()

  resp <- httr::GET(url = url,
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
  # Requests will be qeued if more than 60 requests per second.
  request_time <- as.numeric(Sys.time() - time_stamp)
  if(request_time < 1){
    Sys.sleep(1.05 - request_time)
  }

  pages_to_get <- min(c(max_pages, cont_obj$last_page))

  # Initialise a progress bar
  pb <- txtProgressBar(min = 1, max = pages_to_get, style = 3)

  while(page<cont_obj$last_page && page<max_pages) {
    page <- page + 1

    time_stamp <- Sys.time()

    if(country_code %in% c("EU", "NE")){
      url <- paste0("https://alsi.gie.eu/api?continent=", country_code, "&size=300&page=", page)
    } else {
      url <- paste0("https://alsi.gie.eu/api?country=", country_code, "&size=300&page=", page)
    }

    resp <- httr::GET(url = url,
                      httr::add_headers("x-key" = api_key))

    if(httr::status_code(resp) != 200){
      status_httr <- httr::http_status(resp)
      stop(paste("Category:", status_httr$category,
                 "Reason:", status_httr$reason,
                 "Message:", status_httr$message))
    }

    cont <- httr::content(resp, as = "text", encoding = "UTF-8")

    cont_obj <- jsonlite::fromJSON(cont)

    cont_df <- dplyr::bind_rows(cont_df, cont_obj$data)

    # Requests will be qeued if more than 60 requests per second.
    request_time <- as.numeric(Sys.time() - time_stamp)
    if(request_time < 1){
      Sys.sleep(1.05 - request_time)
    }
    setTxtProgressBar(pb, page)
  }

  cont_df
}


#' Construct the url.
#'
#' @param hostname Hostname
#' @param continent Continent
#' @param country Country
#' @param company Company
#' @param facility Facility
#' @param page Page
#' @param size Size. Defaults to 300.
#' @param scheme Scheme. Defaults to 'https'
#' @param path Path. Defaults to 'api'
#'
url_api_construct <- function(hostname, continent = NULL, country = NULL,
                              company = NULL, facility = NULL, page = NULL,
                              size = 300, scheme = "https", path = "api"){

  url <- list(
    scheme = "https",
    hostname = "agsi.gie.eu",
    path = "api",
    query = list(
      continent = continent,
      country = country,
      company = company,
      facility = facility,
      page = page,
      size = size
    )
  )

  class(url) <- "url"

  url <- httr::build_url(url)

  url
}


#' Get the data from the API endpoint.
#'
#' @param hostname Hostname
#' @param continent Continent
#' @param country Country
#' @param company Company
#' @param facility Facility
#' @param page Page
#' @param max_pages Max pages.
#' @param api_key Api key
#'
gie_pagination_api <- function(hostname, continent = NULL, country = NULL,
                           company = NULL, facility = NULL, page = NULL,
                           max_pages = 5000, api_key = NULL) {

  if(is.null(api_key)){
    api_key <- Sys.getenv("GIE_PAT")
  }

  url <- url_api_construct(hostname = hostname,
                           continent = continent,
                           country = country,
                           company = company,
                           facility = facility)

  time_stamp <- Sys.time()

  resp <- httr::GET(url = url,
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
  # Requests will be qeued if more than 60 requests per second.
  request_time <- as.numeric(Sys.time() - time_stamp)
  if(request_time < 1){
    Sys.sleep(1.05 - request_time)
  }

  pages_to_get <- min(c(max_pages, cont_obj$last_page))

  # Initialise a progress bar
  pb <- txtProgressBar(min = 1, max = pages_to_get, style = 3)

  while(page<cont_obj$last_page && page<max_pages) {
    page <- page + 1

    time_stamp <- Sys.time()

    url <- url_api_construct(hostname = hostname,
                             continent = continent,
                             country = country,
                             company = company,
                             facility = facility,
                             page = page)

    resp <- httr::GET(url = url,
                      httr::add_headers("x-key" = api_key))

    if(httr::status_code(resp) != 200){
      status_httr <- httr::http_status(resp)
      stop(paste("Category:", status_httr$category,
                 "Reason:", status_httr$reason,
                 "Message:", status_httr$message))
    }

    cont <- httr::content(resp, as = "text", encoding = "UTF-8")

    cont_obj <- jsonlite::fromJSON(cont)

    cont_df <- dplyr::bind_rows(cont_df, cont_obj$data)

    # Requests will be qeued if more than 60 requests per second.
    request_time <- as.numeric(Sys.time() - time_stamp)
    if(request_time < 1){
      Sys.sleep(1.05 - request_time)
    }
    setTxtProgressBar(pb, page)
  }

  cont_df
}

