#' @title addLocation
#' @description Function translates (if possible) cell numbers into coordinates or coordinates
#' into cell numbers and adds them to the spatial dimension of the object.
#' @param x magpie object to be enriched with spatial information. Currently only works
#' for 0.5 degree data sets with 59199 or 67420 cells or a subset of it.
#' @param fillMissing if NULL cells missing from the total 59199 are just being ignore. If set to a value
#' missing cells will be added with this value (e.g. all set to 0 if fillMissing is 0)
#' @param naCellNumber number or string given as cell number for cells which do not have an assigned cell
#' number in the 59199 mapping
#' @param format either "both" or "iso.grid". When set to "iso.grid", the function will remove the pre-existing
#' spatial dimensions after adding the coordinates' sorted iso.grid location. This argument is ignored for input
#' objects initially containing iso.grid and LPJ datasets.
#' @return magpie object with added spatial subdimensions
#' @author Jan Philipp Dietrich, Felicitas Beier, Michael Crawford
#' @examples
#'
#'  map <- Cell2Country()
#'  x <- y <- z <- population_magpie
#'
#'  getCoords(x) <- map[100:109, c("lon", "lat")]
#'  getItems(x, "i") <- NULL
#'  addLocation(x)
#'
#'  getCoords(y) <- map[100:109, c("lon", "lat")]
#'  getItems(y, "i") <- NULL
#'  addLocation(y, format = "iso.grid")
#'
#'  getCells(z) <- map$celliso[100:109]
#'  addLocation(z)
#'
#' @importFrom magclass hasCoords getItems getCoords dimExists dimCode ncells clean_magpie getCells getSets
#' @export

addLocation <- function(x, fillMissing = NULL, naCellNumber = 0, format = "both") {
    .hasCells   <- function(x, map59199) return(all(getItems(x, dim = 1) %in% map59199$cell))
    .hasCellISO <- function(x, map59199) return(all(getItems(x, dim = 1) %in% map59199$celliso))
    .has59199   <- function(x) return(ncells(x) == 59199 &&
                                          dimExists(1.2, x) &&
                                          all(sort(as.integer(getItems(x, dim = 1.2, full = TRUE))) == 1:59199))
    .has67420   <- function(x) return(ncells(x) == 67420)

    map59199 <- Cell2Country()
    map67420 <- readRDS(system.file("extdata", "mapLPJcells2Coords.rds", package = "magpiesets"))

    x <- clean_magpie(x, what = "sets")
    if (hasCoords(x)) {

        co <- getCoords(x)
        names(co) <- c("lon", "lat")
        co$inData <- TRUE
        map59199$cell <- 1:dim(map59199)[1]
        selFull <- merge(map59199, co, all = TRUE)
        sel     <- selFull[!is.na(selFull$inData), 1:6]
        if (anyNA(sel$cell)) {
            sel$cell[is.na(sel$cell)] <- naCellNumber
            sel$iso[is.na(sel$iso)] <- "NA"
        }
        rownames(sel) <- paste0(sel$lon, "#", sel$lat)
        sel <- sel[paste0(co$lon, "#", co$lat), ]
        getItems(x, dim = "country", maindim = 1) <- sel$iso
        getItems(x, dim = "cell",   maindim = 1) <- sel$cell
        if (!is.null(fillMissing)) {
            selMissing <- selFull[is.na(selFull$inData), 1:6]
            if (nrow(selMissing) > 0) {
                xAdd <- x[rep(1, nrow(selMissing)), , ]
                xAdd[, , ] <- fillMissing
                getItems(xAdd, dim = "country") <-  selMissing$iso
                getItems(xAdd, dim = "cell")    <-  selMissing$cell
                x <- mbind(x, xAdd)
            }
        }

        if (format == "iso.grid") {
            currentDims <- unlist(strsplit(getSets(x, fulldim = FALSE)[1], "\\."))
            desiredDims <- c("country", "cell")
            x <- collapseDim(x, dimCode(x = x, dim = setdiff(currentDims, desiredDims)))
            getSets(x)[c("d1.1", "d1.2")] <- c("iso", "grid")
            x <- magpiesort(x)
        }

    } else if (.hasCells(x, map59199) || .hasCellISO(x, map59199) || .has59199(x)) {

        if (.hasCells(x, map59199))  {
            i <- "cell"
        } else if (.hasCellISO(x, map59199)) {
            i <- "celliso"
        } else {
            i <- "other"
            map59199$other <- getItems(x, dim = 1, full = TRUE)[order(as.integer(getItems(x, 1.2, full = TRUE)))]
        }
        rownames(map59199) <- map59199[[i]]
        getCoords(x) <- map59199[getItems(x, dim = 1), c("lon", "lat")]

        if (format != "both") {
            stop("The argument \"format\" is ignored for this input data format.")
        }

    } else if (.has67420(x)) {

        # transform lpjcells to coordinates
        getItems(x, dim = "x", maindim = 1)   <- gsub("(.*)\\.(.*)", "\\1", map67420$coords)
        getItems(x, dim = "y", maindim = 1)   <- gsub("(.*)\\.(.*)", "\\2", map67420$coords)
        getItems(x, dim = "iso", maindim = 1) <- map67420$iso
        getSets(x)[1] <- "N"

        if (format != "both") {
            stop("The argument \"format\" is ignored for this input data format.")
        }

    } else {
        stop("Cannot handle this case!")
    }

    return(x)
}
