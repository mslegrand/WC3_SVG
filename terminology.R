library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(devtools)
library(assertthat)

url<-"http://www.w3.org/TR/SVG/intro.html#Terminology"

script<-getURL(url)
doc <- htmlParse(script)
getNodeSet(doc, "//dl/dt")->ns.dt
getNodeSet(doc, "//dl/dd")->ns.dd
sapply(ns.dt, xmlValue)->val.dt
sapply(ns.dd, xmlValue)->val.dd

l1<-lapply(4:length(val.dt), function(i){
  tmp<-getNodeSet(ns.dd[[i]],"a/span[@class='element-name']")
  tmpVal<-sapply(tmp,xmlValue)
  data.table(term=val.dt[i], role="elem", name=tmpVal)
})
tmpVal<-NULL
l2<-lapply(4:length(val.dt), function(i){
  tmp<-getNodeSet(ns.dd[[i]],"a/span[@class='attr-name']")
  tmpVal<-sapply(tmp,xmlValue)
  data.table(term=val.dt[i], role="attr", name=tmpVal)
})

terminology.DT<-rbindlist(c(l1,l2))
terminology.DT$name->tmp
sapply(tmp, function(x)x[[1]])->tmp
gsub('[‘’]','',tmp)->tmp
terminology.DT$name<-tmp



write.table(terminology.DT,"dataTableLink Terminology.csv",sep=",",row.names=FALSE,quote=FALSE)

#CAN WE DO SOMETHING WITH xmlAncestors??
