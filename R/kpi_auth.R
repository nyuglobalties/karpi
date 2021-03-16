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

#' Fetch or set KPI Auth Token
#'
#' By default, this function reads from the "KPI_TOKEN" environment
#' variable. If `token` is provided, then the "KPI_TOKEN" environment
#' variable will be overwriten with the value of `token`.
#'
#' @param token If not `NULL`, the value with which to overwrite the
#'   "KPI_TOKEN" environment variable.
#' @return The value of "KPI_TOKEN" or `NULL` if not defined
#' @export
kpi_auth_token <- function(token = NULL) {
  stopifnot(is.character(token) || is.null(token))

  if (!is.null(token)) {
    Sys.setenv(KPI_TOKEN = token)
  } else {
    token <- Sys.getenv("KPI_TOKEN") %if_empty_string% NULL
  }

  token
}
