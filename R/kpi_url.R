kpi_set_url <- function(type = "kobotoolbox", domain = NULL) {
  type <- match.arg(type, c("kobotoolbox", "humanitarian", "custom"))

  default_domains <- c(
    kobotoolbox = "kf.kobotoolbox.org",
    humanitarian = "kc.humanitarianresponse.info"
  )

  if (type != "custom") {
    url <- default_domains[type]
  } else {
    stopifnot(is.character(domain))

    url <- domain
  }

  if (!identical(url, getOption("kpi.urlroot"))) {
    options(kpi.urlroot = url)
  }

  url
}

kpi_url <- function() {
  url <- getOption("kpi.urlroot")

  if (is.null(url)) {
    stop0("Please set KPI URL root with `kpi_set_url()`")
  }

  url
}
