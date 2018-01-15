#' @title reportingReverse
#' @description reverses the reporting format with tree structure (e.g. "Demand","Demand|Agriculture|Food (Mt DM)") into a conventional magpie format
#' 
#' @export
#' 
#' @param x a MAgPIE which has the format of a report file (e.g. "Demand|Agriculture|Food (Mt DM)")
#' @return a MAgPIE object with higher dimensionailty
#' @author Benjamin Leon Bodirsky
#' @examples
#'   \dontrun{
#'     budget<-calcOutput("ValidDemand")
#'     reportingReverse(budget)
#'   }


reportingReverse<-function(x){
  
  # check for correct format
  
  check<-gregexpr(" \\(", getNames(x))
  if(any(unlist(check)==-1)){stop("names do not contain units. Seems to be buggy format, not supported.")}
  if(any(lapply(check,length)>1)){stop("Containts ' (' outside of units. Seems to be buggy format, not supported.")}
  check<-gregexpr(")", getNames(x))
  if(any(lapply(check,length)>1)){stop("Containts ')' outside of units. Seems to be buggy format, not supported.")}
  
  newnames=getNames(x)

  # add unit dimension
  
  newnames<-gsub( newnames ,pattern = " \\(",replacement = "\\.")
  newnames<-gsub(newnames,pattern = "\\)",replacement = "")  

  # replace lines by dots
  
  newnames<-gsub(newnames,pattern = "\\|",replacement = "\\.")
  
  # add incomplete dimensions
  
  namevector<-strsplit(newnames,split = "\\.")
  dimensions=unlist(lapply(namevector,FUN=length))

  add_missing<-function(z){
    if(length(z)<max(dimensions)){
      z=paste(c(z[1:(length(z)-1)],rep(z[(length(z)-1)],times=max(dimensions)-length(z)),z[length(z)]),collapse = ".")
    } else {
      z=paste(z,collapse = ".")
    }
    return(z)
  }

  newnames<-unlist(lapply(namevector,add_missing))

  getNames(x)<-newnames
  x<-clean_magpie(x)
  getSets(x)[length(getSets(x))]<-"unit"
  return(x)
}


'reportingReverse<-function(x,reversenames=FALSE,select=NULL,namedims=3,endoftree=FALSE){
  indicatordim=length(fulldim(x)[[1]])-2
  
  #selection of subset of tree
  if(!is.null(select)){
    namevector<-getNames(x,dim=indicatordim)
    beginswith<-function(name,pattern=select){
      any(pmatch(x = pattern,table = name)==1)
    }
    x<-x[,,namevector[which(unlist(lapply(X = namevector,FUN=beginswith))==T)]]
    getNames(x, dim=indicatordim)<-substring(getNames(x, dim=indicatordim),first = nchar(select)+1)
    if(nchar(select)>0){
      namedims=namedims-length(strsplit(select,split = "\\|")[[1]])
    }
  }

  if(endoftree){
    namevector<-strsplit(getNames(x,dim=indicatordim),split = "\\|")
    test<-unlist(lapply(namevector,length))
    if ((namedims-1)>min(test)){stop("namedims is higher than the end of tree for certain branches. Use select to choose a branch.")}
    take_end<-function(x){
      out<-paste(c(x[0:(namedims-1)],x[length(x)]),collapse = ".")
    }
    newnames<-unlist(lapply(namevector,take_end))
    if(any(duplicated(newnames))){stop("endoftree includes duplicated names. use namedims = number")}
    newnamesnames<-paste(paste("data",1:(min(test)+1),sep=""),collapse = ".")
  } else {
    namevector<-strsplit(getNames(x,dim=indicatordim),split = "\\|")
    test<-unlist(lapply(namevector,length))
    x<-x[,,getNames(x,dim=indicatordim)[test==namedims]]
    
    namevector<-strsplit(getNames(x,dim=indicatordim),split = "\\|")
    test<-unique(unlist(lapply(namevector,length)))
    if (length(test)>1) {stop("Entries in reporting object have different number of subdimensions. Use namedims parameter to get rid of this. Subdimensions found: ",paste(test,collapse = ", "))}
    
    newnames<-gsub(getNames(x,dim=indicatordim),pattern = "\\|",replacement = "\\.")
    newnamesnames<-paste(paste("data",1:test,sep=""),collapse = ".")  
  }
  getNames(x,dim=indicatordim)<-newnames
  names(dimnames(x))[[3]]<-paste(c(getSets(x)[3:(length(getSets(x))-1)],newnamesnames),collapse = ".")
  if(reversenames){
    for(i in indicatordim:(length(fulldim(x)[[1]])-2)){
      getNames(x,dim=i)<-reportingnames(getNames(x,dim=i),from = "reportingnames",to="magpienames")
    }
  }
  return(x)
}'