`%??%` <- function(x, y) if (is.null(x)) y else x

`%if_empty_string%` <- function(x, y) {
  stopifnot(is.character(x), length(x) == 1)

  if (x == "") y else x
}

stop0 <- function(...) {
  stop(..., call. = FALSE)
}

cat_line <- function(x = NULL, .env = parent.frame()) {
  cat(glue(x, .envir = .env), "\n", sep = "")
}
