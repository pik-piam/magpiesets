#' @title addLocation
#' @description Function translates (if possible) cell numbers into coordinates or coordinates 
#' into cell numbers and addes them to the spatial dimension of the object.
#' @param x magpie object to be enriched with spatial information. Currently only works
#' for 0.5 degree data sets with 59199 cells or a subset of it.
#' @return magpie object with added spatial subdimensions
#' @author Jan Philipp Dietrich
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
#' @importFrom magclass hasCoords getItems getCoords
#' @export

addLocation <- function(x){
  .hasCells   <- function(x,map) return(all(getItems(x,dim = 1) %in% map$cell) )
  .hasCellISO <- function(x,map) return(all(getItems(x,dim = 1) %in% map$celliso))
  
  map <- Cell2Country()
  
  if (hasCoords(x)) {
    co <- getCoords(x)
    names(co) <- c("lon","lat")
    map$cell <- 1:dim(map)[1]
    sel <- merge(map, co, all.y = TRUE)
    if (anyNA(sel$cell)) {
      sel$cell[is.na(sel$cell)] <- 0
      sel$iso[is.na(sel$iso)] <- "NA"
    }
    rownames(sel) <- paste0(sel$lon,"#",sel$lat)
    sel <- sel[paste0(co$lon,"#",co$lat),]
    getItems(x,dim = "country",maindim = 1) <- sel$iso
    getItems(x,dim = "cell",   maindim = 1) <- sel$cell
  } else if (.hasCells(x,map) || .hasCellISO(x,map)) {
    if (.hasCells(x,map)) i <- "cell"
    else i <- "celliso"
    rownames(map) <- map[[i]]
    getCoords(x) <- map[getItems(x,dim = 1),c("lon","lat")]
  } else {
    stop("Cannot handle this case!")
  }
  return(x)
}
