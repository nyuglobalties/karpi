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
#' @param drop_expanded Drops multiple choices columns that were expanded into
#'   dummies.
#' @param include_geolocation If `TRUE`, keeps "_geolocation" in data. Defaults
#'   to `FALSE`.
#' @param system_vars Which ODK system variables to keep in the downloaded
#'   dataset, e.g. "_uuid", "_id", and "_version". "_geolocation" is omitted
#'   for privacy reasons by default.
#' @param missing_vars What to do if variables expected in the KoBo XForm meta
#'   are missing. Defaults to nothing. Usually this is the result of empty
#'   or unused fields during data collection.
#' @return A data.frame of the dataset stored as `asset_id` on KoBo
#'
#' @examples
#' \dontrun{
#' # Form url is this: https://kf.kobotoolbox.org/#/forms/abcdefghijklmnop
#' # Asset ID is: abcdefghijklmnop
#' karpi::kpi_get_data("abcdefghijklmnop")
#' }
#' @export
kpi_get_data <- function(asset_id,
                         expand_multiple = TRUE,
                         expand_delimiter = ".",
                         drop_expanded = FALSE,
                         include_geolocation = FALSE,
                         system_vars = "_uuid",
                         missing_vars = c("nothing", "warn", "msg", "err")) {
  raw_data <- fetch_data(asset_id)
  meta <- kpi_get_xlsform(asset_id)

  # Action for when missing variables (dropped due to lack
  # of data) are encountered
  missing_vars <- match.arg(missing_vars)

  # Add group names to variables
  meta <- rename_vars_with_groups(meta)

  # Remove geolocation for privacy concerns
  if (isTRUE(include_geolocation)) {
    raw_data <- strip_geolocation(raw_data)
  }

  # Extract data columns
  raw_data <- keep_survey_vars(raw_data, meta, system_vars, missing_vars)

  # Bind data
  data <- lapply(raw_data, data.table::as.data.table)
  data <- as.data.frame(
    data.table::rbindlist(data, use.names = TRUE, fill = TRUE)
  )

  if (isTRUE(expand_multiple)) {
    expand_multiple_choice(
      data,
      meta,
      delimiter = expand_delimiter,
      drop_cols = drop_expanded
    )
  } else {
    data
  }
}

#' Download a dataset in its JSON form
#'
#' Downloads a survey dataset with ID `asset_id` from KoBo, leaving the data
#' in its list-recursive format. This routine returns the data as-is: no
#' standard data processing occurs by default.
#'
#' @param asset_id The survey's asset ID (found after the "/forms/" in the URL)
#' @return A list of the dataset stored as `asset_id` on KoBo
#'
#' @examples
#' \dontrun{
#' # Form url is this: https://kf.kobotoolbox.org/#/forms/abcdefghijklmnop
#' # Asset ID is: abcdefghijklmnop
#' karpi::kpi_get_raw_data("abcdefghijklmnop")
#' }
#' @export
kpi_get_raw_data <- function(asset_id) {
  fetch_data(asset_id)
}

strip_geolocation <- function(res) {
  lapply(res, function(r) {
    r[["_geolocation"]] <- NULL

    r
  })
}

keep_survey_vars <- function(res, meta, system_vars, miss_act) {
  stopifnot(inherits(meta, "odk_xlsform"))

  meta$survey <- meta$survey[
    !meta$survey$type %in% c("begin_group", "end_group", "note"),
  ]
  keep_vars <- c(meta$survey$name, system_vars)

  total_pool <- unlist(lapply(res, names))

  if (!all(keep_vars %in% total_pool)) {
    vars_not_in_pool <- keep_vars[!keep_vars %in% total_pool]
    notif <- glue("Missing variables: {vec_view(vars_not_in_pool)}")

    switch(miss_act,
      msg = message(notif),
      warn = warn0(notif),
      err = stop0(notif)
    )

    keep_vars <- keep_vars[keep_vars %in% total_pool]
  }

  lapply(res, function(r) {
    r[keep_vars]
  })
}

#' Utility function that expands ODK multiple choice columns
#'
#' For `select_multiple` variables, multiple responses are encoded
#' as space-delimited entries in each cell. This routine creates
#' dummy columns (1-hot encoded) for each possible response. This
#' is included as a step in [karpi::kpi_get_data].
#'
#' @param dat A data.frame, probably rbound data from the Kobo API
#' @param meta Associated metadata for this dataset, probably from
#'   [karpi::kpi_get_xlsform].
#' @param delimiter The character delimiting the parent variable name
#'   from the child response value
#' @param drop_cols If `TRUE`, columns that have been expanded will
#'   be dropped.
#' @return `dat` with expanded multiple choice columns
#' @export
expand_multiple_choice <- function(dat,
                                   meta,
                                   delimiter = ".",
                                   drop_cols = FALSE) {
  survey_df <- meta$survey

  survey_df <- survey_df[grepl("^select_multiple", survey_df$type), ]

  if (nrow(survey_df) == 0) {
    return(dat)
  }

  survey_df$choice <- gsub("^select_multiple\\s+", "", survey_df$type)

  for (col in survey_df$name) {
    # Presumbly, the missing_vars action is "nothing" here
    if (!col %in% names(dat)) {
      next
    }

    choice_id <- survey_df[survey_df$name == col, "choice"]
    choices <- meta$choices[meta$choices$list_name == choice_id, "name"]

    dat <- insert_multiple_choice_dummies(dat, col, choices, delimiter)

    if (isTRUE(drop_cols)) {
      dat[[col]] <- NULL
    }
  }

  dat
}

insert_multiple_choice_dummies <- function(dat, col, choices, delim) {
  pivot <- which(names(dat) == col)
  before <- names(dat)[1:(pivot - 1)]

  if (pivot == length(dat)) {
    after <- NULL
  } else {
    after <- names(dat)[(pivot + 1):length(dat)]
  }

  new_cols <- paste0(col, delim, choices)
  dat[, new_cols] <- NA_integer_

  for (choice in choices) {
    new_col <- paste0(col, delim, choice)
    selected <- !is.na(dat[[col]])

    dat[selected, new_col] <- as.integer(grepl(choice, dat[selected, col]))
  }

  dat[, c(before, col, new_cols, after)]
}

rename_vars_with_groups <- function(meta, sep = "/") {
  group_stack <- kv_stack()

  for (i in seq_len(nrow(meta$survey))) {
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
    stop0(
      "Improperly formatted XLSForm. Group not closed: ",
      peek(group_stack)$key
    )
  }

  meta
}

rename_meta_rows <- function(meta, rows, group_name, sep) {
  meta$survey[rows, "name"] <- paste0(
    group_name,
    sep,
    meta$survey[rows, "name"]
  )
  meta
}
