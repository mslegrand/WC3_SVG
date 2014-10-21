#presentationAttributes
library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(devtools)
library(assertthat)
source("showMe.R")


# Todo: 
#   - convert ors (ie. |)  into something  useful
#   - identify the elements enclosed by anlge brackets, ie. shape...

getPresLinkInfo<-function(doc, attr, loc){
  ex<-(attr=='color-rendering' & loc=='ColorRenderingProperty')
  #browser(expr=ex)
  getNodeSet(doc, paste("//*[@id='",loc,"']",sep=""))->ns.loc
  showMe(attr,loc)
  parent<-xmlParent(ns.loc[[1]])
  getNodeSet(parent, "dd/table/tr")->desc1.ns
  
  descriptions<-sapply( desc1.ns, xmlValue)
  gsub("\\s+"," ", descriptions)->tmp
  strsplit(tmp,":")->tmp2
  gsub("\\s+", ".", sapply(tmp2, function(x)x[[1]]))->names
  sapply(tmp2, function(x)x[[2]])->dirtyValues
  if(!(inherits(dirtyValues,"character"))){
    showMe(attr, loc)
    showMe(tmp)
    showMe(class(dirtyValues))
  }
  assert_that(inherits(dirtyValues,"character"))
  
  str_trim( dirtyValues )->values
  sapply(values, function(x){substr(x, 3,nchar(x))})->value
  as.list(c(attr, value))->value
  names(value)<-c('attr',names)
  dt<-as.data.table(value)
  #setnames(dt, names)
  dt  
}

getPresentationLinkInfo<-function(page, la.dt){
  url<-paste("http://www.w3.org/TR/SVG/",page,sep="")
  showMe(url)
  script<-getURL(url)
  doc <- htmlParse(script)
  
  #la.dt[, getPresLinkInfo(doc,  attr, loc) ]
  #lapply(locs, function(loc)getPresLinkInfo(doc, loc))->dts
  lapply(1:nrow(la.dt), function(i)getPresLinkInfo(doc, la.dt$attr[i], la.dt$loc[i]))->dts
  #lapply(1:nrow(tmp.df), function(i)paste(tmp.df$x[i],tmp.df$y[i]) )
  
  rbindlist(dts)  
}


getPresAttrs<-function(){
  url="http://www.w3.org/TR/SVG/attindex.html"    
  script<-getURL(url)
  doc <- htmlParse(script)
  getNodeSet(doc, "//table")->ns.tab
#   getNodeSet(ns.tab[[2]],"tr")->ns.tab.tr
#   ns.tab.tr[[1]]
  getNodeSet(ns.tab[[2]],"tr/td")->ns.tab.tr.td
  getNodeSet(ns.tab.tr.td[[1]], "a")->pa.nodes

  sapply(pa.nodes,xmlValue)->presAttr
  gsub("[‘’]", "",presAttr)->presAttr

  sapply(pa.nodes, xmlAttrs)->presLink
  unlist(sapply(strsplit(presLink,"#"), function(x)x[1]))->linkPage
  unlist(sapply(strsplit(presLink,"#"), function(x)x[2]))->linkLoc

  pa.dt<-data.table(attr=presAttr, link=presLink, page=linkPage, loc=linkLoc)
  pages<-unique(pa.dt$page)

  #pge<-pages[1]
  
  get.presatt<-function(pge){
    pa.dt[page==pge, list(attr, loc)]->la.dt
    showMe(length(la.dt))
    getPresentationLinkInfo(pge, la.dt)->tmp
    showMe(length(tmp))    
    tmp    
  }
  presatt.list<-lapply(pages, get.presatt)
  presatt.table<-rbindlist(l = presatt.list)
  
  #get.presatt(pge)
#   presatt.list<-lapply(linkPages, function(pge){
#     pa.dt[page==pge, list(attr, loc)]->la.dt
#     showMe(length(la.dt))
#     getPresentationLinkInfo(pge, la.dt)->tmp
#     showMe(length(tmp))    
#     tmp
#   })
# 
#   presatt.table<-rbindlist(l = presatt.list)

#   pge<-linkPage[1]
#   #locs<-pa.dt[linkPage==page,]$linkLoc
# #   locs<-pa.dt[linkPage==page,]$loc
# #   attrs<-pa.dt[linkPage==page,]$attr
#   pa.dt[page==pge, list(attr, loc)]->la.dt
#   getPresentationLinkInfo(pge, la.dt)->tmp
#   tmp


#   url<-paste("http://www.w3.org/TR/SVG/",page,sep="")
#   script<-getURL(url)
#   doc <- htmlParse(script)
#   locs<-pa.dt[linkPage==page,]$linkLoc
# 
#   loc<-locs[1]
#   loc<-'TextAnchorProperty'
#   getNodeSet(doc, paste("//*[@id='",loc,"']",sep=""))->ns.loc
#   parent<-xmlParent(ns.loc[[1]])
#   getNodeSet(parent, "dd/table/tr")->desc1.ns
#   
#   descriptions<-sapply( desc1.ns, xmlValue)
#   gsub("\\s+"," ", descriptions)->tmp
#   strsplit(tmp,":")->tmp2
#   gsub("\\s+", ".", sapply(tmp2, function(x)x[[1]]))->names
#   str_trim( sapply(tmp2, function(x)x[[2]]) )->values
#   sapply(values, function(x){substr(x, 3,nchar(x))})->value
#   names(values)<-names
#   dt<-as.data.table(as.list(value))
#   setnames(dt, names)
#   dt

  


  
  
}
  
getPresAttrs()->tabs