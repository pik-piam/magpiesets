#' @title Cell2Country
#' @description function returning the mapping between grid and country
#'
#' @export
#'
#' @return data frame returning the mapping between cells and countries
#' @author Edna J. Molina Bacca
#' @examples
#'
#'   \dontrun{
#'    Cell2Country()
#'   }
#'

Cell2Country <- function(){
  mapping <- readRDS(system.file("extdata", "CountryToCellMapping.Rda", package="magpiesets"))
  return(mapping)
}
