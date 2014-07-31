library(XML)
library(RCurl)


getDefinitions<-function(){
  url<-"http://www.w3.org/TR/SVG/intro.html#TermGraphicsElement" #gives the properties
  
  script<-getURL(url)
  doc <- htmlParse(script)
  ns.dt<-getNodeSet(doc, "//dt")
  ns.dd<-getNodeSet(doc, "//dd")
  
  defs<-sapply(ns.dt, xmlValue)
  
  kids<-function(n)xmlChildren(n)
  sapply(ns.dd, kids)->tmp2
  
  # fn.sp<-function(i){
  #   getNodeSet(ns.dd[[i]]
  #   tmp.ns<-getNodeSet(n,"//span")
  #   sapply(n, xmlValue)
  # }
  
  lapply(ns.dd, getChildrenStrings)->tmp3
  
  getA<-function(i){
    tmp3[[i]]->tt
    tt[which(names(tt)=="a")]->tt
    tt<-gsub("[‘’]","",tt)
    names(tt)<-NULL
    tt
  }
  
  lapply(4:length(tmp3), getA)->tmp4
  defs<-defs[4:length(defs)] #this is a list of the definitions
  names(tmp4)<-defs
  tmp4
}

getElementTypes<-function(defs){
  elementTypes<-defs[grep('element|shape',names(defs))] #need to add shape
  #elementTypes<-defs[elementIndx]
  names<-gsub("\n     ", "", names(elementTypes))
  names<-gsub("^ +", "", names)
  names<-gsub(" element", "", names)
  names(elementTypes)<-names
  elementTypes 
}

defs<-getDefinitions()
elementTypes<-getElementTypes(defs)



#for newViewports
# The ‘svg’ element
# A ‘symbol’ element define new viewports whenever they are instanced by a ‘use’ element.
# An ‘image’ element that references an SVG file will result in the establishment of a temporary 
# new viewport since the referenced resource by definition will have an ‘svg’ element.
# A ‘foreignObject’ element creates a new viewport for rendering the content that is within the element.


