#' @title addLocation
#' @description Function translates (if possible) cell numbers into coordinates or coordinates 
#' into cell numbers and addes them to the spatial dimension of the object.
#' @param x magpie object to be enriched with spatial information. Currently only works
#' for 0.5 degree data sets with 59199 or 67420 cells or a subset of it.
#' @return magpie object with added spatial subdimensions
#' @author Jan Philipp Dietrich, Felicitas Beier
#' @examples
#'  map <- Cell2Country()
#'  x <- y <- population_magpie
#'  
#'  getCoords(x) <- map[100:109,c("lon","lat")]
#'  getItems(x,"i") <- NULL
#'  addLocation(x)
#'  
#'  getCells(y) <- map$celliso[100:109]
#'  addLocation(y)
#'  
#'  
#' @importFrom magclass hasCoords getItems getCoords dimExists ncells clean_magpie getCells
#' @export

addLocation <- function(x){
  .hasCells   <- function(x,map59199) return(all(getItems(x,dim = 1) %in% map59199$cell) )
  .hasCellISO <- function(x,map59199) return(all(getItems(x,dim = 1) %in% map59199$celliso))
  .has59199   <- function(x) return(ncells(x) == 59199 &&  
                                    dimExists(1.2,x) && 
                                    all(sort(as.integer(getItems(x,dim = 1.2,full = TRUE))) == 1:59199))
  .has67420   <- function(x) return(ncells(x) == 67420)
  
  map59199 <- Cell2Country()
  map67420 <- readRDS(system.file("extdata", "mapLPJcells2Coords.rds", package="magpiesets"))
  
  x <- clean_magpie(x, what = "sets")
  if (hasCoords(x)) {
    co <- getCoords(x)
    names(co) <- c("lon","lat")
    map59199$cell <- 1:dim(map59199)[1]
    sel <- merge(map59199, co, all.y = TRUE)
    if (anyNA(sel$cell)) {
      sel$cell[is.na(sel$cell)] <- 0
      sel$iso[is.na(sel$iso)] <- "NA"
    }
    rownames(sel) <- paste0(sel$lon,"#",sel$lat)
    sel <- sel[paste0(co$lon,"#",co$lat),]
    getItems(x,dim = "country",maindim = 1) <- sel$iso
    getItems(x,dim = "cell",   maindim = 1) <- sel$cell
  } else if (.hasCells(x,map59199) || .hasCellISO(x,map59199) || .has59199(x)) {
    if (.hasCells(x,map59199)) i <- "cell"
    else if(.hasCellISO(x,map59199)) i <- "celliso"
    else {
      i <- "other"
      map59199$other <- getItems(x,dim=1,full=TRUE)[order(as.integer(getItems(x,1.2,full=TRUE)))]
    }
    rownames(map59199) <- map59199[[i]]
    getCoords(x) <- map59199[getItems(x,dim = 1),c("lon","lat")]
  } else if (.has67420(x)) {
    # transform lpjcells to coordinates
    getCells(x) <- paste(map67420$coords, map67420$iso, sep=".")
  } else {
    stop("Cannot handle this case!")
  }
  return(x)
}
