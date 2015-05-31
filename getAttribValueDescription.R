library(XML)
library(RCurl)
library(data.table)
library(stringr)

# To retrieve 3 things:
# attribute (name of attribute)
# attribute values (names of attribute values)
# attribute value definition

#check out http://zvon.org/comp/r/ref-SVG_1_1_Full.html#Attributes~filterUnits

#url="http://www.w3.org/TR/SVG/animate.html#AttributeNameAttribute"

scrapeOnePage<-function(url){
  cat("processing:",url,"\n")
  script<-getURL(url)
  doc <- htmlParse(script)
  
  #2nd try
  adef.ns<-getNodeSet(doc,"//dt/span[@class='adef']")
  adef.parent<-sapply(adef.ns,xmlParent)  
  adef.next.sib<-sapply(adef.parent,getSibling)
  
  
  getdtddVal<-function(n){
    child.dd.ns<-getNodeSet(n, "*/dd")
    if(length(child.dd.ns)>0){
      ddval<-lapply(child.dd.ns, xmlValue)
      child.dt.ns<-sapply(child.dd.ns, function(x)getSibling(x, after=F))
      dtval<-sapply(child.dt.ns, xmlValue)
      #names(ddval)<-dtval
    } else {
      dtval<-"??"
      ddval<-xmlValue(n)
    }
    #ddval<-paste( dtval,":",ddval)
    ddval<-gsub('\\n','',ddval)
    ddval<-gsub(' +',' ',ddval)
    list(dtval=dtval, ddval=ddval)
  }
  
  #obtain a vector of attribute names
  sapply(adef.ns,xmlValue)->attrib.name
  
  #obtain a matrix whose 
  # first row is the value name
  # 2nd row is the value description
  sapply(adef.next.sib, getdtddVal)->vals
  
  #
  attr.value.name.summary<-sapply(adef.parent, function(n){ 
    tmp.ns<-getNodeSet(n, "span[@class='attr-value']")
    vals<-sapply(tmp.ns, xmlValue)
    vals<-paste(vals, collapse=" ")
    vals}
  )
  
  value.names.list<-vals[1,]
  value.def.list<-vals[2,]
  indx<-which(value.names.list=="??")
  value.names.list[indx]<-attr.value.name.summary[indx]
  indx<-union( which(is.na(value.names.list)), which(is.null(value.names.list)) )
  indx<-union(indx, which(nchar(value.names.list)==0))
  value.parent<-sapply(adef.parent,xmlValue)
  id.parent<-sapply(adef.parent, function(n){
    tmp<-xmlAttrs(n)
    tmp[["id"]]->idAttr
    if(is.null(idAttr)){
      idAttr<-'NA'
    }
    idAttr
  }
                    )
  tmp<-str_split(value.parent,"=")
  value.names.list[indx]<-lapply(tmp[indx], function(x) x[2])
  indx<-union( which(is.na(value.names.list)), which(is.null(value.names.list)) ) 
  value.names.list[indx]<-'badValue'
  #value.names.list<-lapply(value.names.list, function(x)paste0("*(",nchar(x),")*"))
  
  
  # the attribute name is given by the xmla value of a node in adef.ns
  # the attribute values names are listed in attr.value.name.summary
  # the attribute values are individually listed in 
  
  tmp<-lapply(1:length(value.names.list),
              function(i)
                data.table(loc=id.parent[i], attr=attrib.name[i], value=value.names.list[[i]], value.def=value.def.list[[i]])
  )
  
  DT<-rbindlist(tmp)
  
}


getAttrValDesc<-function(){ 
  url="http://www.w3.org/TR/SVG/attindex.html"
  getHTMLLinks(url)->links
  links<-links[grep("#",links)]
  
  str_split(links, "#")->links
  unique(sapply(links, function(x)x[1]))->links
  pages<-paste0("http://www.w3.org/TR/SVG/",links)
  pages<-pages[-c(1,18)]
  lapply(pages, scrapeOnePage)->AttValDesc.list
  rbindlist(AttValDesc.list)->AVD.DT

  
  AVD.DT
}

AVD.DT<-getAttrValDesc()

cleanData<-function(x){
  x<-gsub("\\s+"," ",x)
  x<-gsub("%","",x)
}

AVD.DT[, ':='(value=cleanData(value), value.def=cleanData(value.def)) ]

# We manually intervene here to add some missing Attribute values

ClassAttribute
PreserveAspectRatioAttribute
StyleAttribute
SystemLanguageAttribute
TextPathElementHrefAttribute
TextPathElementMethodAttribute
TextPathElementSpacingAttribute
TransformAttribute
ViewBoxAttribute
XLinkActuateAttribute
XLinkArcRoleAttribute
XLinkRoleAttribute
XLinkShowAttribute
XLinkTitleAttribute
XLinkTypeAttribute
ZoomAndPanAttribute
feConvolveMatrixElementKernelUnitLengthAttribute



write.table(AVD.DT,file="dataTableLink/AVDTable.tsv",
            sep="\t",
            row.names=FALSE,
            quote=FALSE)

#NOTE: comparing with attributes in es.DT, we are missing 
#  "transform" "viewBox"   "origin1"   "in1"

#comparing with AVE