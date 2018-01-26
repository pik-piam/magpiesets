#' @title reporthelper
#' @description Aggregates MAgPIE products to standard reporting categories and changes to reporting names. 
#' Automatically recognizes if only a reduced form of "kall" is provided.
#' 
#' @export
#' @import magclass
#' 
#' @param x Magpie object with data that shall be reported
#' @param dim Dimension in which magpie products ("tece" etc) can be found
#' @param level_zero_name the general reporting name of the Magpie object (e.g. "Agricultural Production")
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param sort sort items in 3rd dimension alphabetically (TRUE or FALSE)
#' @param partly boolean or set name, that should be reported in detail, even if it is just partly provided within the gdx
#' @return MAgPIE object with aggregated and renamed items in 3rd dimension
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- calories(level="regglo", products="kcr",attributes="protein")
#'     x <- reporthelper(x)
#'   }
#'   

reporthelper<-function(x,dim=3.1,level_zero_name="All products", detail=TRUE, sort=FALSE, partly=FALSE){
  
  dim2=as.numeric(substring(dim,3))
  
  #Set partly values
  set_partly <- logical(14) 
  set_names  <- c("kall", "crops_excluding_bioenergy_and_forage", "cereals", "oilcrops", "sugarcrops", "other_crops", "bioenergycrops", "foddr", "pasture", "ksd", "kres", "kli", "fish","kforest")
  names(set_partly)  <- set_names
  
  if(!is.logical(partly)){
    set_partly[partly] <- TRUE
  } else if(partly==TRUE){
    set_partly[]       <- TRUE
  }
  
  rename_it<-function(report,set,prefix="",groupname=T,subitems=F, partly=FALSE){
  
    elements <- findset(set,noset = "original")
    
    #reported set have to match elements of choosen set exactly or partly options has to be choosen  
    if( (all(elements%in%getNames(x,dim=dim2)) & partly==FALSE) |
        (any(elements%in%getNames(x,dim=dim2)) & partly==TRUE)  ){
      
      if(partly==TRUE) elements <- intersect(elements, getNames(x,dim=dim2))
      
      if((groupname)){groupname=reportingnames(set)}else{groupname=""}
      
      if(prefix!=""&groupname!=""){prefix=paste0(prefix,"|")}else{prefix=prefix}
      
      tmp1<-dimSums(x[,,elements],dim=dim)
      
      if (length(fulldim(x)[[1]])>3) {
        tmp1<-add_dimension(x = tmp1,dim=dim,add = "products",paste0(prefix,groupname))
      } else {
        getNames(tmp1,dim=dim2)<-paste0(prefix,groupname)
      }
      
      if (subitems==T){
        tmp2<-x[,,elements]
        getNames(tmp2,dim=dim2)<-paste0(prefix,groupname,"|",reportingnames(getNames(tmp2,dim=dim2)))
        tmp1<-mbind(tmp1,tmp2)
      } 
      
    #report nothing otherwise 
    } else {tmp1<-NULL}
    
    if (is.null(tmp1)){
      return (report)
    } else {
      return(mbind(report,tmp1))
    }
  }
  
  out<-rename_it(report=NULL,set="kall",subitems = F, prefix=level_zero_name, groupname = F, partly=unname(set_partly["kall"]))
  
  out<-rename_it(report=out,set="crops_excluding_bioenergy_and_forage",subitems = F, prefix=level_zero_name, groupname = T, partly=unname(set_partly["crops_excluding_bioenergy_and_forage"]))
  
  if(level_zero_name==""){
    prefix<-reportingnames("crops_excluding_bioenergy_and_forage")
  } else {
    prefix<-paste0(level_zero_name,"|",reportingnames("crops_excluding_bioenergy_and_forage"))  
  }
  
  out<-rename_it(report=out,set="cereals",subitems = detail, prefix=prefix, groupname = T, partly=unname(set_partly["cereals"]))
  out<-rename_it(report=out,set="oilcrops",subitems = detail, prefix=prefix, groupname = T, partly=unname(set_partly["oilcrops"]))
  out<-rename_it(report=out,set="sugarcrops",subitems = detail, prefix=prefix, groupname = T, partly=unname(set_partly["sugarcrops"]))
  out<-rename_it(report=out,set="other_crops",subitems = detail, prefix=prefix, groupname = T, partly=unname(set_partly["other_crops"]))
  
  out<-rename_it(report=out,set="bioenergycrops",subitems = F, prefix=level_zero_name, groupname = T, partly=unname(set_partly["bioenergycrops"]))
  out<-rename_it(report=out,set="foddr",subitems = F, prefix=level_zero_name, groupname = T, partly=unname(set_partly["foddr"]))
  out<-rename_it(report=out,set="pasture",subitems = F, prefix=level_zero_name, groupname = T, partly=unname(set_partly["pasture"]))
  
  out<-rename_it(report=out,set="ksd",subitems = detail, prefix=level_zero_name, groupname = T, partly=unname(set_partly["ksd"]))
  
  out<-rename_it(report=out,set="kres",subitems = detail, prefix=level_zero_name, groupname = T, partly=unname(set_partly["kres"]))
  
  out<-rename_it(report=out,set="kli",subitems = detail, prefix=level_zero_name, groupname = T, partly=unname(set_partly["kli"]))
  out<-rename_it(report=out,set="fish",subitems = F, prefix=level_zero_name, groupname = T, partly=unname(set_partly["fish"]))
  
  out<-rename_it(report=out,set="kforest",subitems = detail, prefix=level_zero_name, groupname = T, partly=unname(set_partly["kforest"]))
  
  
  if(sort) out <- out[,,sort(getNames(out))]
  
  return(out) 
}