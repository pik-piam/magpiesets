#' @title reportingnames
#' @description function returning the reporting names of a vector
#' 
#' @export
#' @import magclass
#' @importFrom utils read.csv
#' 
#' @param x a vector of common magpie names (e.g. c("tece","trce"))
#' @param from magpienames for common magpie abbreviations, reportingnames for reverse translation
#' @param to reportingnames for official reporting names, reportincolors for typical colorcode of an item
#' @param mapping csv file in inst/extdata folder.
#' @return vector with reporting names or volors
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder, Patrick v. Jeetze
#' @examples
#' 
#'   \dontrun{
#'     reportingnames("tece")
#'     reportingnames("tece",to="reportingcolors")
#'   }
#'   

reportingnames <- function(x,from="magpienames",to="reportingnames",mapping="reportingnames.csv"){
  #setwd("D:/MAgPIE SVN/libraries/magpiesets/inst/extdata")
  mapping <- read.csv(system.file("extdata",mapping="reportingnames.csv",package = "magpiesets"))
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
