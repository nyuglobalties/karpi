kpi_asset_info <- function(asset_id) {
  path <- glue("/api/v2/assets/{asset_id}.json")
  res <- kpi_get(path)

  jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8"),
    simplifyVector = FALSE,
  )
}
