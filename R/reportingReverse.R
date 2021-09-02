#' @title reportingReverse
#' @description reverses the reporting format with tree structure (e.g. "Demand",
#' "Demand|Agriculture|Food (Mt DM)") into a conventional magpie format
#'
#' @export
#'
#' @param x a MAgPIE which has the format of a report file (e.g. "Demand|Agriculture|Food (Mt DM)")
#' @return a MAgPIE object with higher dimensionailty
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' budget <- calcOutput("ValidDemand")
#'     reportingReverse(budget)
#' }
#'
reportingReverse <- function(x) {

  # check for correct format

  check <- gregexpr(" \\(", getNames(x))
  if (any(unlist(check) == -1)) {
    stop("names do not contain units. Seems to be buggy format, not supported.")
  }
  if (any(lapply(check, length) > 1)) {
    stop("Containts ' (' outside of units. Seems to be buggy format, not supported.")
  }
  check <- gregexpr(")", getNames(x))
  if (any(lapply(check, length) > 1)) {
    stop("Containts ')' outside of units. Seems to be buggy format, not supported.")
  }

  newnames <- getNames(x)

  # add unit dimension

  newnames <- gsub(newnames, pattern = " \\(", replacement = "\\.")
  newnames <- gsub(newnames, pattern = "\\)", replacement = "")

  # replace lines by dots

  newnames <- gsub(newnames, pattern = "\\|", replacement = "\\.")

  # add incomplete dimensions

  namevector <- strsplit(newnames, split = "\\.")
  dimensions <- unlist(lapply(namevector, FUN = length))

  addMissing <- function(z) {
    if (length(z) < max(dimensions)) {
      z <- paste(c(z[1:(length(z) - 1)], rep(z[(length(z) - 1)], times = max(dimensions) - length(z)),
                   z[length(z)]), collapse = ".")
    } else {
      z <- paste(z, collapse = ".")
    }
    return(z)
  }

  newnames <- unlist(lapply(namevector, addMissing))

  getNames(x) <- newnames
  x <- clean_magpie(x)
  getSets(x)[length(getSets(x))] <- "unit"
  return(x)
}
