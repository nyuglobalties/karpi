#' Get survey asset metadata
#' 
#' Fetches the overall asset metadata for a survey with
#' asset ID `asset_id`
#' 
#' @param asset_id The survey's asset ID (found after the "/forms/" in the URL)
#' @return A list based on the JSON structure of the asset metadata
#' @examples 
#' \dontrun{
#'   # Form url is this: https://kf.kobotoolbox.org/#/forms/abcdefghijklmnop
#'   # Asset ID is: abcdefghijklmnop
#'   karpi::kpi_asset_info("abcdefghijklmnop")
#' }
#' @export
kpi_asset_info <- function(asset_id) {
  path <- glue("/api/v2/assets/{asset_id}.json")
  res <- kpi_get(path)

  jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8"),
    simplifyVector = FALSE,
  )
}
