#' @title addLocation
#' @description Function translates (if possible) cell numbers into coordinates or coordinates 
#' into cell numbers and addes them to the spatial dimension of the object.
#' @param x magpie object to be enriched with spatial information. Currently only works
#' for 0.5 degree data sets with 59199 cells or a subset of it.
#' @return magpie object with added spatial subdimensions
#' @author Jan Philipp Dietrich
#' @examples
#'  map <- Cell2Country()
#'  x <- population_magpie
#'  getCoords(x) <- map[100:109,c("lon","lat")]
#'  
#' @importFrom magclass hasCoords getItems getCoords
#' @export

addLocation <- function(x){
  .hasCells <- function(x,map) return(all(getItems(x,dim=1) %in% c(map$cell,map$celliso)))
  
  map <- Cell2Country()
  
  if(hasCoords(x)) {
    if(.hasCells(x,map)) {
      message("Nothing to do here as coordinates as well as cells are already available.")
      return(x)
    }
    co <- getCoords(x)
    names(co) <- c("lon","lat")
    map$cell <- 1:dim(map)[1]
    selection <- merge(map,co)
    getItems(x,dim="country",maindim=1) <- selection$iso
    getItems(x,dim="cell",maindim=1) <- selection$cell
    return(x)
  } else if(.hasCells(x,map)) {
    
  } else {
    stop("Neither Coordinates nor cell numbers available!")
  }
}
