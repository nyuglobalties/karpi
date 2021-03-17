`%??%` <- function(x, y) if (is.null(x)) y else x

`%if_empty_string%` <- function(x, y) {
  stopifnot(is.character(x), length(x) == 1)

  if (x == "") y else x
}

stop0 <- function(...) {
  stop(..., call. = FALSE)
}

warn0 <- function(...) {
  warning(..., call. = FALSE, immediate. = TRUE)
}

cat_line <- function(x = NULL, .env = parent.frame()) {
  cat(glue(x, .envir = .env), "\n", sep = "")
}

vapply_mold <- function(.type) {
  function(.x, .f, ...) {
    vapply(.x, .f, vector(.type, 1L), ...)
  }
}

vcapply <- vapply_mold("character")
vlapply <- vapply_mold("logical")
viapply <- vapply_mold("integer")

vec_view <- function(x, max_len = 10) {
  if (is.character(x)) {
    chr_x <- glue("'{x}'")
  } else {
    chr_x <- as.character(x)
  }

  if (!is.na(max_len) && length(chr_x) > max_len) {
    chr_x[[max_len + 1]] <- "..."
    chr_x <- chr_x[1:(max_len + 1)]
  }

  glue("[{glue_collapse(chr_x, ', ')}]")
}