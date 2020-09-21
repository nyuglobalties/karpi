kpi_get <- function(path, type = "application/json") {
  stopifnot(is.character(path))

  url <- httr::modify_url(glue("https://{kpi_url()}"), path = path)
  res <- httr::GET(
    url,
    combine_headers(kpi_auth_header())
  )

  if (content_type(res) != type) {
    stop0("Unexpected API return type: ", content_type(res))
  }

  if (httr::status_code(res) != 200) {
    stop0("API error: ", httr::status_code(res))
  }

  res
}
