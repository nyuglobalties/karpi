#' Set KPI host domain
#' 
#' karpi supports the kobotoolbox (kf) and humanitarian (kc) domains
#' by default. To point to another KPI domain, set `type` to "custom"
#' and set `domain` to the domain URL. Setting the URL overwrites the
#' "kpi.urlroot" option.
#' 
#' @param type Which KPI platform to use - one of "kobotoolbox", "humanitarian", or "custom"
#' @param domain Domain URL for "custom" type platform
#' @return The KPI domain URL
#' 
#' @details # Default KPI domains
#' KoBo has two domains for use:
#' 
#' 1. kobotoolbox (kf.kobotoolbox.org): General purpose KoBo deployments on KoBo
#' 1. humanitarian (kc.kobotoolbox.info): Unrestricted KoBo platform used by humanitarian agencies (e.g. UNICEF)
#' 
#' @export 
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

#' Fetch current KPI domain URL
#' 
#' Reads from the "kpi.urlroot" option. If the value is `NULL`,
#' an error is raised to notify the user to set the URL with
#' `kpi_set_url()`.
#' 
#' @return The KPI domain URL
#' @export 
kpi_url <- function() {
  url <- getOption("kpi.urlroot")

  if (is.null(url)) {
    stop0("Please set KPI URL root with `kpi_set_url()`")
  }

  url
}
