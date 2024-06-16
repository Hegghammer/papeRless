# papeRless: Access the Paperless-ngx API From R

**papeRless** is an R package for [Paperless-ngx](https://docs.paperless-ngx.com/), an open-source document management system. The package provides basic access to the Paperless-ngx [REST API](https://docs.paperless-ngx.com/api/) so that you can interact with a Paperless-ngx instance from within R. The package is in early development and only contains functions for select REST API operations.

## Use

Generate a data frame with metadata on all the documents in the Paperless-ngx instance:

```R
## NOT RUN
library(papeRless)
df <- get_all_docs_df()
```

Search a Paperless-ngx instance:

```
ids <- search_docs("london")
```

Download a document from a Paperless-ngx instance:

```
download_doc(ids[1])
```

## Requirements

An API key (aka "API Auth Token") to a Paperless-ngx instance. Get it from the web UI by clicking the user dropdown menu in the top right corner, selecting "My Profile" and clicking the circular arrow button. For a smoother user experience, store the API key and the base URL to the Paperless-ngx instance as the environment variables `PAPERLESS_API_KEY` and `PAPERLESS_BASE_URL` in `.Renviron`.

## Installation

Install the latest development version from Github:

```R
devtools::install_github("hegghammer/papeRless")
```
