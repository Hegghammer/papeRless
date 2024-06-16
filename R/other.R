#' Get users info
#'
#' @description Get information on all users on a Paperless-ngx instance in
#' list form.
#'
#' @param base_url string
#' @param api_key string
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' get_users()
#' }
get_users <- function(base_url = get_base_url(),
                      api_key = get_api_key()
                      ) {

  if (!grepl("http", base_url)) {
    stop("URL must include http:// or https://")
  }

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  url <- file.path(base_url, "api/users/")
  req_auth(url, api_key) |> httr2::req_perform() |> httr2::resp_body_json()

}

#' Get correspondents info
#'
#' @description Get information on all correspondents in the Paperless-ngx
#' instance in list form.
#'
#' @param base_url string
#' @param api_key string
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' get_corrs()
#' }
get_corrs <- function(base_url = get_base_url(),
                      api_key = get_api_key()
                      ) {

  if (!grepl("http", base_url)) {
    stop("URL must include http:// or https://")
  }

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  url <- file.path(base_url, "api/correspondents/")
  req_auth(url, api_key) |> httr2::req_perform() |> httr2::resp_body_json()

}

#' Search documents
#'
#' @description Search a Paperless-ngx instance. Returns a vector of document
#' ids if the term is found.
#'
#' @param query string
#' @param base_url string
#' @param api_key string
#'
#' @return integer vector
#' @export
#'
#' @examples
#' \dontrun{
#' search_docs("london")
#' }
search_docs <- function(query,
                        base_url = get_base_url(),
                        api_key = get_api_key()
                        ) {

  if (!(is.character(query) && length(query) == 1)) {
    stop("query must be a single string.")
  }

  if (!grepl("http", base_url)) {
    stop("URL must include http:// or https://")
  }

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  url <- file.path(base_url, "api/documents", "")
  url <- urltools::param_set(url, key = "query", value = query)
  res <- req_auth(url, api_key) |> httr2::req_perform() |> httr2::resp_body_json()
  unlist(res$all)

}

#' Upload document
#'
#' @description Add a document to a Paperless-ngx instance.
#'
#' @param path string
#' @param title string
#' @param created string
#' @param correspondent integer
#' @param document_type integer
#' @param storage_path string
#' @param tags list
#' @param archive_serial_number integer
#' @param owner integer
#' @param set_permissions string
#' @param base_url string
#' @param api_key string
#'
#' @return no output, called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' upload_doc("scan.pdf")
#' }
upload_doc <- function(path,
                       title = NULL,
                       created = NULL,
                       correspondent = NULL,
                       document_type = NULL,
                       storage_path = NULL,
                       tags = NULL,
                       archive_serial_number = NULL,
                       owner = NULL,
                       set_permissions = NULL,
                       base_url = get_base_url(),
                       api_key = get_api_key()
                       ) {

  if (!grepl("http", base_url)) {
    stop("URL must include http:// or https://")
  }

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  url <- file.path(base_url, "api/documents/post_document", "")
  req_auth(url, api_key) |>
    httr2::req_body_multipart(
      document = curl::form_file(path),
      title = title,
      created = created,
      correspondent = correspondent,
      document_type = document_type,
      storage_path =  storage_path,
      tags = tags,
      archive_serial_number = archive_serial_number,
      owner = owner,
      set_permissions = set_permissions
      ) |>
    httr2::req_perform()
}
