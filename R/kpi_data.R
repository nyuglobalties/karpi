fetch_data <- function(asset_id) {
  path <- glue("api/v2/assets/{asset_id}/data.json")
  res <- kpi_get(path)

  jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8"),
    simplifyVector = FALSE,
  )$results
}

#' Download a dataset
#' 
#' Downloads a survey dataset with ID `asset_id` from KoBo.
#' 
#' @param asset_id The survey's asset ID (found after the "/forms/" in the URL)
#' @param expand_multiple If true, each choice in a multiple choice 
#'   field will get its own column. Expanding uses one-hot encoding.
#' @param expand_delimiter The expanded columns follow the pattern
#'   "[field name][delimiter][choice value]". By default, the delimiter
#'   is "." in karpi. Other ODK implementations use "/" as a delimiter.
#' @param system_vars Which ODK system variables to keep in the downloaded
#'   dataset, e.g. "_uuid", "_id", and "_version". "_geolocation" is omitted
#'   for privacy reasons.
#' @return A data.frame of the dataset stored as `asset_id` on KoBo
#' 
#' @examples 
#' \dontrun{
#'   # Form url is this: https://kf.kobotoolbox.org/#/forms/abcdefghijklmnop
#'   # Asset ID is: abcdefghijklmnop
#'   karpi::kpi_get_data("abcdefghijklmnop")
#' }
#' @export
kpi_get_data <- function(
  asset_id,
  expand_multiple = TRUE,
  expand_delimiter = ".",
  system_vars = "_uuid"
) {
  raw_data <- fetch_data(asset_id)
  meta <- kpi_get_xlsform(asset_id)

  # Add group names to variables
  meta <- rename_vars_with_groups(meta)

  # Remove geolocation for privacy concerns
  raw_data <- strip_geolocation(raw_data)

  # Extract data columns
  raw_data <- keep_survey_vars(raw_data, meta, system_vars)

  # Bind data
  data <- lapply(raw_data, data.table::as.data.table)
  data <- as.data.frame(data.table::rbindlist(data, use.names = TRUE, fill = TRUE))

  if (isTRUE(expand_multiple)) {
    expand_multiple_choice(data, meta, delimiter = expand_delimiter)
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

  meta$survey <- meta$survey[!meta$survey$type %in% c("begin_group", "end_group", "note"), ]
  keep_vars <- c(meta$survey$name, system_vars)

  lapply(res, function(r) {
    r[keep_vars]
  })
}

expand_multiple_choice <- function(dat, meta, delimiter = ".") {
  survey_df <- meta$survey

  survey_df <- survey_df[grepl("^select_multiple", survey_df$type), ]

  if (nrow(survey_df) == 0) {
    return(dat)
  }

  survey_df$choice <- gsub("^select_multiple\\s+", "", survey_df$type)

  for (col in survey_df$name) {
    choice_id <- survey_df[survey_df$name == col, "choice"]
    choices <- meta$choices[meta$choices$list_name == choice_id, "name"]

    dat[, paste0(col, delimiter, choices)] <- NA_integer_

    for (choice in choices) {
      selected <- !is.na(dat[[col]])

      dat[selected, paste0(col, delimiter, choice)] <- as.integer(grepl(choice, dat[selected, col]))
    }

    dat[[col]] <- NULL
  }

  dat
}

rename_vars_with_groups <- function(meta, sep = "/") {
  group_stack <- kv_stack()

  for (i in 1:nrow(meta$survey)) {
    if (meta$survey$type[i] == "begin_group") {
      push(group_stack, meta$survey$name[i], i + 1L)
      next
    } else if (meta$survey$type[i] == "end_group") {
      group_start <- pop(group_stack)

      meta <- rename_meta_rows(
        meta,
        (group_start$value):(i - 1),
        group_start$key,
        sep
      )
    }
  }

  if (!isempty(group_stack)) {
    stop0("Improperly formatted XLSForm. Group not closed: ", peek(group_stack)$key)
  }

  meta
}

rename_meta_rows <- function(meta, rows, group_name, sep) {
  meta$survey[rows, "name"] <- paste0(group_name, sep, meta$survey[rows, "name"])
  meta
}
