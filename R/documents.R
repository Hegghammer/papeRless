#' Get document ids
#'
#' @description Gets the document ids for all documents on a Paperless-ngx
#' instance.
#'
#' @param base_url string
#' @param api_key string
#'
#' @return integer vector
#' @export
#'
#' @examples
#' \dontrun{
#' get_doc_ids()
#' }
get_doc_ids <- function(base_url = get_base_url(),
                        api_key = get_api_key()
                        ) {

  if (!grepl("http", base_url)) {
    stop("URL must include http:// or https://")
  }

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  url <- file.path(base_url, "api/documents/")
  res <- req_auth(url, api_key) |> httr2::req_perform() |> httr2::resp_body_json()
  unlist(res$all)

}

#' Get document info
#'
#' @description Get information about a document in list form.
#'
#' @param id integer
#' @param base_url string
#' @param api_key string
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' get_doc_info()
#' }
get_doc_info <- function(id,
                         base_url = get_base_url(),
                         api_key = get_api_key()
                         ) {

  if (!(is.integer(id) && length(id) == 1)) {
    stop("id must be a single integer.")
  }

  if (!grepl("http", base_url)) {
    stop("URL must include http:// or https://")
  }

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  url <- file.path(base_url, "api/documents", id, "")
  req_auth(url, api_key) |> httr2::req_perform() |> httr2::resp_body_json()

}

#' Get document info dataframe
#'
#' @description Get information about a document in dataframe form.
#'
#' @param id integer
#' @param base_url string
#' @param api_key string
#' @param n_tags integer. The number of tag columns you want in the data frame.
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_doc_info_df(17)
#' }
get_doc_info_df <- function(id,
                            base_url = get_base_url(),
                            api_key = get_api_key(),
                            n_tags = 5
                            ) {

  if (!(is.integer(id) && length(n_tags) == 1)) {
    stop("id must be a single integer.")
  }

  if (!grepl("http", base_url)) {
    stop("URL must include http:// or https://")
  }

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  if (!(is.numeric(n_tags) && length(n_tags) == 1)) {
    stop("n_tags must be a single integer.")
  }

  res <- get_doc_info(id, base_url, api_key)

  if (is.null(res$id)) {
    id <- NA
  } else {
    id <- res$id
  }

  if (is.null(res$correspondent)) {
    correspondent <- NA
  } else {
    correspondent <- res$correspondent
  }

  if (is.null(res$document_type)) {
    document_type <- NA
  } else {
    document_type <- res$document_type
  }

  if (is.null(res$storage_path)) {
    storage_path <- NA
  } else {
    storage_path <- res$storage_path
  }

  if (is.null(res$title)) {
    title <- NA
  } else {
    title <- res$title
  }

  if (is.null(res$content)) {
    content <- NA
  } else {
    content <- res$content
  }

  df1 <- data.frame(id = id,
                    correspondent = correspondent,
                    document_type = document_type,
                    storage_path = storage_path,
                    title = title,
                    content = content)

  df2 <- data.frame(matrix(NA, nrow = 1, ncol = n_tags))
  names(df2) <- paste0("tag", 1:n_tags)

  if (length(res$tags) > n_tags) {
    for (i in 1:n_tags) {
      df2[i] <- res$tags[[i]]
    }
  } else {
    for (i in seq_along(res$tags)) {
      df2[i] <- res$tags[[i]]
    }
  }

  if (is.null(res$created)) {
      created <- NA
    } else {
      created <- res$created
    }

  if (is.null(res$created_date)) {
      created_date <- NA
    } else {
      created_date <- res$created_date
    }

  if (is.null(res$modified)) {
      modified <- NA
    } else {
      modified <- res$modified
    }

  if (is.null(res$added)) {
      added <- NA
    } else {
      added <- res$added
    }

  if (is.null(res$archive_serial_number)) {
      archive_serial_number <- NA
    } else {
      archive_serial_number <- res$archive_serial_number
    }

  if (is.null(res$original_file_name)) {
      original_file_name <- NA
    } else {
      original_file_name <- res$original_file_name
    }

  if (is.null(res$archived_file_name)) {
      archived_file_name <- NA
    } else {
      archived_file_name <- res$archived_file_name
    }

  if (is.null(res$owner)) {
      owner <- NA
    } else {
      owner <- res$owner
    }

  if (is.null(res$user_can_change)) {
      user_can_change <- NA
    } else {
      user_can_change <- res$user_can_change
    }

  if (is.null(res$is_shared_by_requester)) {
      is_shared_by_requester <- NA
    } else {
      is_shared_by_requester <- res$is_shared_by_requester
    }

  df3 <- data.frame(created = created,
                    created_date = created_date,
                    modified = modified,
                    added = added,
                    archive_serial_number = archive_serial_number,
                    original_file_name = original_file_name,
                    archived_file_name = archived_file_name,
                    owner = owner,
                    user_can_change = user_can_change,
                    is_shared_by_requester = is_shared_by_requester)

  if (length(res$notes) == 0) {
    note <- NA
  } else {
    note <- res$notes[[1]]$note
  }

  df4 <- data.frame(notes = note)

  if (length(res$custom_fields) == 0) {
    df5 <- data.frame(custom_fields = NA)
  } else {
    df5 <- data.frame(matrix(NA, nrow = 1, ncol = length(res$custom_fields)))
    field_codes <- unlist(purrr::map(res$custom_fields, ~ .x$field))
    names(df5) <- paste0("custom_field_", field_codes)
    for (i in seq_along(res$custom_fields)) {
      val <- res$custom_fields[[i]]$value
      if (is.null(val)) val <- NA
      df5[i] <- val
    }
  }
  cbind(df1, df2, df3, df4, df5)
}

#' Get all documents dataframe
#'
#' @description Get a dataframe with information about all the documents on a
#' Paperless-ngx instance (one row per document).
#'
#' @param base_url string
#' @param api_key string
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_all_docs_df()
#' }
get_all_docs_df <- function(base_url = get_base_url(),
                            api_key = get_api_key()
                            ) {

  if (!grepl("http", base_url)) {
    stop("URL must include http:// or https://")
  }

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  ids <- get_doc_ids(base_url, api_key)
  print(paste("Processing doc 1 of", length(ids), ".."))
  df <- get_doc_info_df(ids[1], base_url, api_key)
  for (i in 2:length(ids)) {
    print(paste("Processing doc", i, "of", length(ids), ".."))
    row <- get_doc_info_df(ids[i], base_url, api_key)
    df <- rbind(df, row)
  }
  df
}

#' Download document
#'
#' @description Downloads a document.
#'
#' @param id integer
#' @param destdir string
#' @param base_url string
#' @param api_key string
#'
#' @return no output, called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' download_doc(17)
#' }
download_doc <- function(id,
                    destdir = getwd(),
                    base_url = get_base_url(),
                    api_key = get_api_key()
) {

  if (!(is.integer(id) && length(id) == 1)) {
    stop("id must be a single integer.")
  }

  if (!(dir.exists(destdir))) {
    stop("destdir must be a valid directory path.")
  }

  if (!grepl("http", base_url)) {
    stop("URL must include http:// or https://")
  }

  if (!(is.character(api_key) && nchar(api_key) == 40)) {
    stop("API key must be a 40-character string.")
  }

  res <- get_doc_info(id, base_url, api_key)
  filename <- gsub(" ", "_", res$archived_file_name)
  path <- file.path(destdir, filename)
  url <- file.path(base_url, "documents", id, "download/")
  resp <- req_auth(url, api_key) |> httr2::req_perform()
  writeBin(resp$body, path)
  print(paste("Downloaded", filename, "in", destdir, "."))

}
