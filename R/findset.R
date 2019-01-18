#' @title findset
#' @description function returning set item of MAgPIE sets
#' 
#' @export
#' @import magclass
#' @importFrom utils read.csv
#' 
#' @param set a MAgPIE set (e.g. "kcr"), or a vector of sets.
#' @param alias if TRUE, set elements are extended by '_alias'. Can be used to avoid doubling of dimnames.
#' @param noset if "original", a set that has no mapping is returned without changes; otherwhise a vector of length 0 is returned, plus a warning that the set does not exist.
#' @param version Switch between different version of the magpiesets library (use 'versionset' to load version types itself)
#' @return MAgPIE set items
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder, Kristine Karstens
#' @examples
#'   \dontrun{
#'     findset("kcr")
#'   

findset <- function(set, alias=FALSE, noset="warning", version=NULL){
 
  default_version <- "default_sep18"
  out <- NULL
  
  #Load relationmatrix
  relationmatrix <- read.csv(system.file("extdata","magpiesets.csv",package = "magpiesets"))
  
  #Read in all sets
  for(i in 1:length(set)){
    setx <- set[i]
    setx <- as.character(relationmatrix$all[which(as.character(relationmatrix[,which(names(relationmatrix)==setx)])==setx)])
    if (length(setx)==0){
      if (noset=="original"){
        setx <- set[i]
      } else {
        warning(paste0("The set ",set[i]," does not exist in magpiesets.csv"))
      }
    }
    out  <- c(out,setx)
  }
  
  ############## start identifiy version settings ############## 
  
  #Identify version set from 'parameter' or 'option'
  #Test, if 'parameter' is undefined
  if(is.null(version)){
    
    #Test if 'option' is undefined
    if(is.null(magpiesets_version <- getOption("magpiesets_version"))){
      
      #If both 'parameter' and 'option' are undefined, use default
      version <- default_version
    } else {
      
      #Use 'option', if just this in defined
      version <- magpiesets_version 
    }
    
  } else {
    
    #Just check further if version is no 'versionset'
    if(version !="versionset"){
      
      #If both 'parameter' and 'option' are defined, print warning. 'parameter' will be used.
      if(!is.null(magpiesets_version <- getOption("magpiesets_version"))){
        warning(paste0("Version set from magpiesets is defined as parameter (",version,") and as option (",magpiesets_version,
                       ").\nParameter used: ",version))
      } 
    }
    #If just 'parameter' is defined, 'version' variable is already set correctly
  }
  
  #Avoid endless recursive function calls of 'findset' for version sets 
  if(version!="versionset"){ 
    
    #Test, if version is known and if not, set to default value
    if(identical(AllKnowItems <- findset(version, version="versionset"),character(0))){
      stop(paste("Unknown version set",version))
      AllKnowItems <- findset(default_version, version="versionset")
    }
    
    #Shrink to items know at that version
    set  <- intersect(out, AllKnowItems)
    
    if(identical(set, character(0)) & (noset=="original") ){
      set <- out
    }
    
  } else { 
    set  <- out
  }
  
  ############## end identifiy version settings   ############## 
  
  
  if(alias==TRUE) {set<-paste0("alias_",set)} else if (alias==FALSE) {setx<-set} else {set<-paste0(alias,"_",set)}
  
  return(set)
}
