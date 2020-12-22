#' @title FRA names
#' @description function returning the reporting names from FRA 2020 category
#' 
#' @export
#' @import magclass
#' @importFrom utils read.csv
#' 
#' @param x a vector of common FRA 2020 names (e.g. c("nat_reg","agb"))
#' @param from fra names for common magpie abbreviations, FRAnames for reverse translation
#' @param to Common names for official reporting names, reportincolors for typical colorcode of an item
#' @param mapping csv file in inst/extdata folder.
#' @return vector with reporting names or volors
#' @author Abhijeet Nishra
#' @examples
#' 
#'   \dontrun{
#'     FRAnames("nat_reg")
#'     FRAnames("tece",to="reportingcolors")
#'   }
#'   

FRAnames <- function(x,from="franames",to="FRAnames",mapping="fra_names.csv"){
  #setwd("D:/MAgPIE SVN/libraries/magpiesets/inst/extdata")
  mapping <- read.csv(system.file("extdata",mapping="fra_names.csv",package = "magpiesets"))
  mapping_from <- as.vector(mapping[,from])
  mapping_to <- as.vector(mapping[,to])
  names(mapping_to) <- mapping_from
  if (any ((x %in% mapping_from)==FALSE)) {
    warning("Following categories in data had no mapping entry: ",paste(unique(x[which((x %in% mapping_from)==FALSE)]),collapse = ' '))
  }    
  result <- mapping_to[x]
  nomapping<-which(is.na(result))
  result[nomapping]<-x[nomapping]
  return(result)
}
