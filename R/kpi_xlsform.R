fetch_xlsform <- function(asset_id) {
  path <- glue("/api/v2/assets/{asset_id}.xls")
  res <- kpi_get(
    path,
    type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet; charset=utf-8"
  )

  httr::content(res, as = "raw")
}

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

  structure(
    list(
      survey = survey,
      choices = choices,
      settings = settings
    ),

    class = "odk_xlsform"
  )
}


