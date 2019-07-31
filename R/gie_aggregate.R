#' Get the aggregated historical data export for Europe or Non Europe
#'
#' @param area eu for Europe, ne for Non Europe.
#' @param api_key The default is NULL and searches for your GIE_PAT in you .Renviron
#'     file.
#'
#' @export
#'
#' @examples {
#'
#' library(gie)
#'
#' ne <- gie_aggregate("ne")
#' eu <- gie_aggregate("eu")
#'
#' }
#'
gie_aggregate <- function(area, api_key = NULL){

  area <- tolower(area)

  if(!area %in% c("ne", "eu") & length(area) > 1){
    stop("Area only accepts 'eu' or 'ne' and not both.")
  }

  if(is.null(api_key)){
    api_key <- Sys.getenv("GIE_PAT")
  }

  url <- paste0("https://agsi.gie.eu/api/data/", area)

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
  cont_df$info <- sapply(cont_df$info, function(x){
    if(length(x) < 1){
      x <- as.character(NA)
    } else {
      x <- paste(x, collapse = ";")
    }
    x
  })
  cont_df <- suppressMessages(readr::type_convert(cont_df))

  cont_df
}
