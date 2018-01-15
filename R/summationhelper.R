#' @title summationhelper
#' @description Modifies name of variables in a magpie object to add summation symbols.
#'
#' @export
#' @import magclass
#' @importFrom utils read.csv tail
#' 
#' @param x MAgPIE object from validation calculations in "almost" ready format. for e.g. from x <- calcOutput("ValidDemand","FAO")
#' @param sep summation symbol (+,++,-)
#' @param dim Dimension in which the modification, should take place
#' @param check Switch to turn off checking routine, if FALSE. Default is TRUE.
#' @return MAgPIE object
#' @author Abhijeet Mishra, Kristine Karstens
#' @examples
#' 
#'   \dontrun{
#'     x <- calcOutput("LanduseInitialisation",aggregate = FALSE)
#'     getNames(x) <- paste0("Land Cover|", reportingnames(getNames(x))," (million ha)")   
#'     x <- summationhelper(x)
#'   }
#' 

summationhelper <- function(x, sep = "+", dim = 3.1, check = TRUE){
  
  if(!is.magpie(x)){
    cat("Object passed to this function is not a MAgPIE object.\n")
  } else {
    # Extract names of mag.obj.
    
    dim_from_3 <- as.numeric(substring(dim,3))
    
    z <- getNames(x , dim = dim_from_3)
    
    # We want to do some manupulations and we need to order alphabetically the names,
    # So we start flushing the names to a data frame where we can easily play around with names
    # and still keeping a track of the sequence in which they appear on calcOutput values.
    temp <- data.frame (old = z)
    temp$id <- seq(from = 1, to = nrow (temp), by = 1) #Can even be done using "rownames" hopefully
    
    # Re-arrange the names because we want to see how and where modifications using "+" sign need to be made.
    # This ordering also helps us to identify which sub-categories (separated by "|") we need to deal with
    temp <- temp[order(temp$old),] 
    
    # This line of code will identify how many "|" exist in the names (without the wrongly placed "|", e.g. as in "Demand|Feed|")
    temp$count <- lengths(regmatches(temp$old, gregexpr("(\\|[a-zA-Z])", temp$old)))
    
    
    # Make the names to be treated as characters (beware that the names are factors at the moment).
    temp$old <- as.character(temp$old) 
    
    sum_symbol <- paste0(sep,"|")
    
    # Run through a for loop to make modifications
    
    add_summation <- function(old, count){
      
      # strsplit returns a list so "unlist" needs to be applied,
      # This pushes the resultant list to a vector.
      # The idea is to split the names, make the changes and then put them back together.
      xx <- unlist(strsplit (old,'\\|'))
      
      # We are interested in the last fragment of names separated by "|". These are the fragments which 
      # signify if a "+" has to be added to names. Hence using "tail" here.
      # We already made a counting of "|" in names. The number fragments are number of "|" + 1  
      xx[count+1] <- paste0(sum_symbol, tail(xx,1))
      
      # Now that the last element of vector has been modified according to our needs,
      # We "collapse" the vector whilst adding a "|" back to original places.
      paste0(xx,collapse="|")
      
    }
    
    # Now we store modified names in a new column
    temp$mod <- mapply(add_summation, temp$old, temp$count)
    
    
    # Now, bring back the names as in the same order as they should be
    fixed_name <- temp[order(temp$id),] 
    
    if(check == TRUE){
      comparison <- data.frame(old = z)
      comparison$old <- as.character(comparison$old)
      comparison$modified <- fixed_name$mod
      
      # check if our modifications have been correct. To compare old with new, we just delete the "|+" we added  
      # and the wrongly placed in "|" at end of string (e.g. as in "Demand|Feed|")
      comparison$old      <- gsub("\\|$","", as.character(comparison$old))
      comparison$modified <- gsub(paste0("\\",sep,"\\|"),"",as.character(comparison$modified))
      
      
      # Make comparison and if everything is in good order, set the old names to new names.
      if (identical(comparison[,1],comparison[,2])){
        #cat("Names after modification to mag.obj. match the old names. Order has been maintained as well.\n")
        #cat("Setting modified names to original mag.obj.\n")
        # Then, set the updated names back to mag.obj.
        getNames(x,dim = dim_from_3) <- fixed_name$mod
      } #else {
      #cat("Names after modification to mag.obj. appear to be either not-similar or in incorrect order. Check required.")
      #}
    } else {
      getNames(x,dim = dim_from_3) <- fixed_name$mod
    }
    
    return(x)
  }
}