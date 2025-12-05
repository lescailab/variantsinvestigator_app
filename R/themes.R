#' Application Themes
#'
#' Define the light and dark themes for the VariantsInvestigator application.
#' These functions return `bslib::bs_theme` objects.
#'
#' @importFrom bslib bs_theme
#' @export
get_light_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#95a5a6",
    success = "#18bc9c",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = "Helvetica Neue",
    heading_font = "Helvetica Neue"
  )
}

#' @rdname get_light_theme
#' @export
get_dark_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#375a7f",
    secondary = "#444444",
    success = "#00bc8c",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = "Helvetica Neue",
    heading_font = "Helvetica Neue"
  )
}
