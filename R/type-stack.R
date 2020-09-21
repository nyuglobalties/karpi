kv_stack <- function(type = integer()) {
  stopifnot(is.atomic(type))

  if (length(type) > 0) {
    type <- type[0]
  }

  env <- new.env()

  env$keys <- character()
  env$vals <- type

  structure(
    env,
    class = "kv_stack"
  )
}

push <- function(st, key, value) {
  stopifnot(inherits(st, "kv_stack"))
  stopifnot(is.character(key) && typeof(value) == typeof(st$vals))

  st$keys <- c(key, st$keys)
  st$vals <- c(value, st$vals)

  invisible(st)
}

isempty <- function(st) {
  stopifnot(inherits(st, "kv_stack"))

  length(st$keys) == 0
}

pop <- function(st) {
  stopifnot(inherits(st, "kv_stack"))

  if (isempty(st)) {
    return(NULL)
  }

  popped_key <- top_key(st)
  popped_val <- top_val(st)

  st$keys <- st$keys[-1]
  st$vals <- st$vals[-1]

  list(key = popped_key, value = popped_val)
}

peek <- function(st) {
  stopifnot(inherits(st, "kv_stack"))

  if (isempty(st)) {
    NULL
  } else {
    list(
      key = top_key(st),
      value = top_val(st)
    )
  }
}

top_key <- function(st) {
  stopifnot(inherits(st, "kv_stack"))

  st$keys[1]
}

top_val <- function(st) {
  stopifnot(inherits(st, "kv_stack"))

  st$vals[1]
}
