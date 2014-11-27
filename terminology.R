library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(devtools)
library(assertthat)

# url<-"http://www.w3.org/TR/SVG/intro.html#Terminology"
# url<-"http:# //www.w3.org/TR/2010/WD-SVG11-20100622/intro.html#TermPresentationAttribute"
url<-"http://www.w3.org/TR/SVG/intro.html"
script<-getURL(url)
doc <- htmlParse(script)
getNodeSet(doc, "//dl/dt")->ns.dt
getNodeSet(doc, "//dl/dd")->ns.dd
sapply(ns.dt, xmlValue)->val.dt
sapply(ns.dd, xmlValue)->val.dd

cleanIt<-function(x){
  x<-gsub('\\n','',x)
  x<-gsub(' +',' ',x)
  x<-str_trim(x)
  x
}


l1<-lapply(4:length(val.dt), function(i){
  tmp<-getNodeSet(ns.dd[[i]],"a/span[@class='element-name']")
  tmpVal<-sapply(tmp,xmlValue)
  tmpVal<-unlist(tmpVal)
  if(is.null(tmpVal))
    tmpVal<-'NULL'
  data.table(category=val.dt[i],  element=tmpVal)
})

elCat.DT<-rbindlist(l1)
elCat.DT<-elCat.DT[element!='NULL']
elCat.DT[,category:=cleanIt(category)]
write.table(elCat.DT,"dataTableLink/ElCat.tsv",sep="\t",row.names=FALSE,quote=FALSE)


tmpVal<-NULL
l2<-lapply(4:length(val.dt), function(i){
  tmp<-getNodeSet(ns.dd[[i]],"a/span[@class='attr-name']")
  tmpVal<-sapply(tmp,xmlValue)
  tmpVal<-unlist(tmpVal)
  if(is.null(tmpVal))
    tmpVal<-'NULL'
  data.table(category=val.dt[i],  attribute=tmpVal)
})

attrCat.DT<-rbindlist(l2)
attrCat.DT<-attrCat.DT[attribute!='NULL']
attrCat.DT[,category:=cleanIt(category)]
write.table(attrCat.DT,"dataTableLink/AttrCat.tsv",sep="\t",row.names=FALSE,quote=FALSE)

# terminology.DT<-rbindlist(c(l1,l2))
# terminology.DT$name->tmp
# sapply(tmp, function(x)x[[1]])->tmp
# gsub('[‘’]','',tmp)->tmp
# terminology.DT$name<-tmp



#write.table(terminology.DT,"dataTableLink/ Terminology.csv",sep=",",row.names=FALSE,quote=FALSE)

#CAN WE DO SOMETHING WITH xmlAncestors??
