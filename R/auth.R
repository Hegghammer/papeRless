#' Get API key from environment variable
#'
#' @description Gets the API key to a Paperless-ngx instance from an
#' environment variable. Requires the environment variable
#' \code{PAPERLESS_API_KEY} to be set in \code{.Renviron}.
#'
#' @return string (if variable set)
#' @export
#'
#' @examples
#' \dontrun{
#' get_api_key()
#' }
get_api_key <- function() {

  key <- Sys.getenv("PAPERLESS_API_KEY")
  if (identical(key, "")) {
    stop("No API key found.")
  }
  key

}

#' Get base URL from environment variable
#'
#' @description Gets the base URL to a Paperless-ngx instance from an
#' environment variable. Requires the environment variable
#' \code{PAPERLESS_BASE_URL} to be set in \code{.Renviron}.
#'
#' @return string (if variable set)
#' @export
#'
#' @examples
#' \dontrun{
#' get_base_url()
#' }
get_base_url <- function() {

  url <- Sys.getenv("PAPERLESS_BASE_URL")
  if (identical(url, "")) {
    stop("No base URL found.")
  }
  url

}

#' Authenticate request
#'
#' @description Creates an httr2 authentication request.
#' @param base_url string
#' @param api_key string
#'
#' @return httr2_request
#' @export
#'
#' @examples
#' \dontrun{
#' req_auth()
#' }
req_auth <- function(base_url = get_base_url(),
                     api_key = get_api_key()
) {

  if (!grepl("http", base_url)) {
    stop("URL must include http:// or https://")
  }

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  httr2::request(base_url) |>
    httr2::req_headers(Authorization = format_token(api_key))

}

#' Format token string for request
#'
#' @description Helper function to format token string for HTTP request.
#' @param api_key string
#'
#' @noRd

format_token <- function(api_key = get_api_key()) {

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  paste("Token", api_key)

}

