fetch_data <- function(asset_id) {
  path <- glue("api/v2/assets/{asset_id}/data.json")
  res <- kpi_get(path)

  jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8"),
    simplifyVector = FALSE,
  )$results
}

kpi_get_data <- function(asset_id,
                     expand_multiple = TRUE,
                     system_vars = "_uuid") {
  raw_data <- fetch_data(asset_id)
  meta <- kpi_get_xlsform(asset_id)

  # Remove geolocation for privacy concerns
  raw_data <- strip_geolocation(raw_data)

  browser()

  # Extract data columns
  raw_data <- keep_survey_vars(raw_data, meta, system_vars)

  # Bind data
  data <- lapply(raw_data, data.table::as.data.table)
  data <- as.data.frame(data.table::rbindlist(data, use.names = TRUE, fill = TRUE))

  if (isTRUE(expand_multiple)) {
    expand_multiple_choice(data, meta)
  } else {
    data
  }
}

strip_geolocation <- function(res) {
  lapply(res, function(r) {
    r[["_geolocation"]] <- NULL

    r
  })
}

keep_survey_vars <- function(res, meta, system_vars) {
  stopifnot(inherits(meta, "odk_xlsform"))
  keep_vars <- c(meta$survey$name, system_vars)

  lapply(res, function(r) {
    r[keep_vars]
  })
}

expand_multiple_choice <- function(dat, meta) {
  survey_df <- meta$survey

  survey_df <- survey_df[grepl("^select_multiple", survey_df$type), ]

  if (nrow(survey_df) == 0) {
    return(dat)
  }

  survey_df$choice <- gsub("^select_multiple\\s+", "", survey_df$type)

  for (col in survey_df$name) {
    choice_id <- survey_df[survey_df$name == col, "choice"]
    choices <- meta$choices[meta$choices$list_name == choice_id, "name"]

    dat[, paste0(col, ".", choices)] <- NA_integer_

    for (choice in choices) {
      selected <- !is.na(dat[[col]])

      dat[selected, paste0(col, ".", choice)] <- as.integer(grepl(choice, dat[selected, col]))
    }

    dat[[col]] <- NULL
  }

  dat
}
