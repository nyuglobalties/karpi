fetch_xlsform <- function(asset_id) {
  path <- glue("/api/v2/assets/{asset_id}.xls")
  res <- kpi_get(
    path,
    type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet; charset=utf-8"
  )

  httr::content(res, as = "raw")
}

#' Fetch the XLSForm for a survey with asset ID `asset_id`
#' 
#' Downloads the current XLSForm for a survey with ID `asset_id`.
#' 
#' @param asset_id The survey's asset ID (found after the "/forms/" in the URL)
#' @param file If not `NULL`, the path where the XLSForm should be saved
#' @param verbose If `TRUE`, echos a message where the file was saved
#' @return An `odk_xlsform` object, a `list()` with `id`, `survey`,
#'   `choices`, and `settings` entries.
#' @examples 
#' \dontrun{
#'   # Form url is this: https://kf.kobotoolbox.org/#/forms/abcdefghijklmnop
#'   # Asset ID is: abcdefghijklmnop
#'   form <- karpi::kpi_get_xlsform("abcdefghijklmnop")
#'   print(form$survey)
#'   print(form$choices)
#' }
#' @export
kpi_get_xlsform <- function(asset_id, file = NULL, verbose = TRUE) {
  bytes <- fetch_xlsform(asset_id)

  if (is.null(file)) {
    file <- tempfile()
    on.exit(unlink(file))
  } else {
    if (isTRUE(verbose)) {
      message(glue("Writing XLSForm for '{asset_id}' to '{file}'"))
    }
  }

  readr::write_file(bytes, file)

  survey <- openxlsx::read.xlsx(file, sheet = "survey")
  choices <- openxlsx::read.xlsx(file, sheet = "choices")
  settings <- openxlsx::read.xlsx(file, sheet = "settings")

  odk_xlsform(asset_id, survey, choices, settings)
}

odk_xlsform <- function(asset_id, survey, choices, settings) {
  structure(
    list(
      id = asset_id,
      survey = survey,
      choices = choices,
      settings = settings
    ),
    class = "odk_xlsform"
  )
}

#' @export
print.odk_xlsform <- function(x, ...) {
  cat_line("<ODK XLSForm: '{x$id}'>")
  cat_line()

  cat_line("Survey:")
  print(x$survey)
  cat_line()

  cat_line("Choices:")
  print(x$choices)
  cat_line()

  cat_line("Settings:")
  print(x$settings)

  invisible(x)
}
