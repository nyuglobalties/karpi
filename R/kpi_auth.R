kpi_auth_header <- function(verbose = TRUE) {
  token <- kpi_auth_token()

  if (is.null(token)) {
    stop0(
      "KPI auth token not set. Use `karpi::kpi_auth_token()` ",
      "or the \"KPI_TOKEN\" environment variable to set it."
    )
  }

  list(Authorization = glue("Token {token}"))
}

kpi_auth_token <- function(token = NULL) {
  stopifnot(is.character(token) || is.null(token))

  if (!is.null(token)) {
    Sys.setenv(KPI_TOKEN = token)
  } else {
    token <- Sys.getenv("KPI_TOKEN") %if_empty_string% NULL
  }

  token
}
