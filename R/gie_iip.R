

#' GIE IIP JSON Feed
#'
#' JSON feed for the Urgent Market Messages sent
#'     in on the GIE REMIT platform, consisting
#'     of Type 2 - Unavailabilities of Gas
#'     Facilities and Type 3 - Other Market
#'     Information UMMs. Default view is data submitted today.
#'
#' @export
#'
gie_iip_recent <- function(){

  df <-
    jsonlite::fromJSON(txt = "https://iip.gie.eu/json",
                     simplifyVector = TRUE,
                     simplifyDataFrame = TRUE,
                     simplifyMatrix = TRUE) %>%
    {.$items} %>%
    dplyr::mutate(content_html =
                    purrr::map(content_html, ~xml2::as_list(xml2::read_html(.x))$html$body$remiturgentmarketmessages$umm)) %>%
    dplyr::pull(content_html) %>%
    purrr::map(~as.data.frame(t(as.matrix(unlist(.x))))) %>%
    dplyr::bind_rows() %>%
    readr::type_convert(locale = readr::locale(tz = "UTC"))

  names(df) <- tolower(names(df))
  names(df) <- stringr::str_replace_all(names(df), "[.]", "_")

  df
}

#' Wrapper for the GIE IIP API
#'
#' @param pages pages.
#'
#' @export
#'
#' @examples
#'
#' gie_iip()
#'
gie_iip <- function(pages = 3){

  base_url <- "https://iip.gie.eu/api?"

  res_iip <- jsonlite::fromJSON(paste0(base_url, "page=1&size=30"))

  last_page <- res_iip$last_page

  if(is.null(pages)){
    pages_to_download <- last_page
  } else if(pages > last_page){
    message("pages param is higher than total number of pages. Reverts to total number of pages.")
    pages_to_download <- last_page
  } else if (pages < 1){
    stop("pages param has to be >= 1.")
  } else {
    pages_to_download <- pages
  }

  iip_list <- vector("list", length = pages_to_download)
  iip_list[[1]] <- gie_iip_data_parser(res_iip$data)

  if(pages_to_download == 1) return(iip_list[[1]])

  # Initialise a progress bar
  pb <- txtProgressBar(min = 1, max = pages_to_download, style = 3)

  for(page in 2:pages_to_download){
    res_iip <- jsonlite::fromJSON(paste0(base_url, "page=", page, "&size=30"))

    iip_list[[page]] <- gie_iip_data_parser(res_iip$data)

    setTxtProgressBar(pb, page)
  }

  iip_df <- dplyr::bind_rows(iip_list)

  iip_df
}

gie_iip_data_parser <- function(df){

  df <-
    df %>%
    tidyr::unnest(reportingEntity, names_sep = "_") %>%
    tidyr::unnest(message, names_sep = "_") %>%
    tidyr::unnest(marketParticipant, names_sep = "_") %>%
    tidyr::unnest(balancingZone, names_sep = "_") %>%
    tidyr::unnest(unavailable, names_sep = "_") %>%
    tidyr::unnest(available, names_sep = "_") %>%
    tidyr::unnest(technical, names_sep = "_") %>%
    tidyr::unnest(asset, names_sep = "_")

  names(df) <- tolower(names(df))

  df
}
