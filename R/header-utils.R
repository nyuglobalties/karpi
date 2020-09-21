combine_headers <- function(...) {
  httr::add_headers(unlist(rlang::dots_list(...)))
}

content_type <- function(res) {
  res$headers[["content-type"]]
}
