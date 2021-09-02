#' @title reporthelper
#' @description Aggregates MAgPIE products to standard reporting categories and changes to reporting names.
#' Automatically recognizes if only a reduced form of "kall" is provided.
#'
#' @export
#' @importFrom magclass ndim getNames mbind add_dimension
#'
#' @param x Magpie object with data that shall be reported
#' @param dim Dimension in which magpie products ("tece" etc) can be found
#' @param level_zero_name the general reporting name of the Magpie object (e.g. "Agricultural Production")
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param sort sort items in 3rd dimension alphabetically (TRUE or FALSE)
#' @param partly boolean or set name, that should be reported in detail, even if it is just partly provided
#' within the gdx
#' @param version Switch between different version of the magpiesets library
#' @return MAgPIE object with aggregated and renamed items in 3rd dimension
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder, Kristine Karstens
#' @examples
#' \dontrun{
#' x <- calories(level = "regglo", products = "kcr", attributes = "protein")
#' x <- reporthelper(x)
#' }
#'
reporthelper <- function(x, dim = 3.1, level_zero_name = "All products", detail = TRUE,  # nolint
                         sort = FALSE, partly = FALSE, version = NULL) {

  dim2 <- as.numeric(substring(dim, 3))

  # Set partly values
  setPartly <- logical(14)
  setNames  <- findset("report_it", version = version)
  names(setPartly)  <- setNames

  if (!is.logical(partly)) {
    setPartly[partly] <- TRUE
  } else if (partly == TRUE) {
    setPartly[]       <- TRUE
  }

  # Renaming function (including renaming of higher level category names)
  renameIt <- function(report, set, prefix = "", groupname = T, subitems = F, partly = FALSE) {

    elements <- findset(set, noset = "original", version = version)

    # reported set have to match elements of choosen set exactly or partly options has to be choosen
    if ((all(elements %in% getNames(x, dim = dim2)) & partly == FALSE) |
      (any(elements %in% getNames(x, dim = dim2)) & partly == TRUE)) {

      if (partly == TRUE) elements <- intersect(elements, getNames(x, dim = dim2))

      if ((groupname)) {
        groupname <- reportingnames(set)
      } else {
        groupname <- ""
      }

      if (prefix != "" & groupname != "") {
        prefix <- paste0(prefix, "|")
      } else {
        prefix <- prefix
      }

      tmp1 <- dimSums(x[, , elements], dim = dim)

      if (ndim(x, dim = 3) > 1) {
        tmp1 <- add_dimension(x = tmp1, dim = dim, add = "products", paste0(prefix, groupname))
      } else {
        getNames(tmp1, dim = dim2) <- paste0(prefix, groupname)
      }

      if (subitems == T) {
        tmp2 <- x[, , elements]
        getNames(tmp2, dim = dim2) <- paste0(prefix, groupname, "|", reportingnames(getNames(tmp2, dim = dim2)))
        tmp1 <- mbind(tmp1, tmp2)
      }

      # report nothing otherwise
    } else {
      tmp1 <- NULL
    }

    if (is.null(tmp1)) {
      return(report)
    } else {
      return(mbind(report, tmp1))
    }
  }

  # Start renaming with set 'kall'
  out <- renameIt(report = NULL, set = "kall", subitems = F, prefix = level_zero_name,
                   groupname = F, partly = unname(setPartly["kall"]))

  # Renaming prefix for subsub-categories
  if (level_zero_name == "") {
    prefix <- reportingnames("crops_excluding_bioenergy_and_forage")
  } else {
    prefix <- paste0(level_zero_name, "|", reportingnames("crops_excluding_bioenergy_and_forage"))
  }

  # Loop over sub- and subsub-categories
  for (item in setdiff(setNames, "kall")) {

    if (item %in% c("cereals", "oilcrops", "sugarcrops", "other_crops")) {
      out <- renameIt(report = out, set = item, subitems = detail, prefix = prefix,
                       groupname = TRUE, partly = unname(setPartly[item]))

    } else if (item %in% c("kli", "ksd", "kres", "bioenergycrops", "kforestry")) {
      out <- renameIt(report = out, set = item, subitems = detail,  prefix = level_zero_name,
                       groupname = TRUE, partly = unname(setPartly[item]))

    } else {
      out <- renameIt(report = out, set = item, subitems = FALSE,  prefix = level_zero_name,
                       groupname = TRUE, partly = unname(setPartly[item]))
    }
  }

  if (sort) out <- out[, , sort(getNames(out))]

  return(out)
}
