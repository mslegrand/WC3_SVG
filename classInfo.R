# tend to use as.quoted, stealing from the way @hadley implements .() in the plyr package.
#library(data.table)
# library(plyr)
# dat <- data.table(x_one=1:10, x_two=1:10, y_one=1:10, y_two=1:10) 
# myfun <- function(name) {
#   one <- paste0(name, '_one')
#   two <- paste0(name, '_two')
#   out <- paste0(name,'_out')
#   as.quoted(paste('list(',out, '=',one, '-', two,')'))[[1]]
# }
#dat[, eval(myfun('x')),]

library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(devtools)
library(assertthat)
source("showMe.R")

#@# we grap all table of type class info from page
  
#   getPresentationLinkInfo<-function(page){
  
#page="painting.html"
extractOnePropPage<-function(page){
  url<-paste("http://www.w3.org/TR/SVG/",page,sep="")
  showMe(url)
  script<-getURL(url)
  doc <- htmlParse(script)
  
  getNodeSet(doc, paste("//table[@class='propinfo']",sep=""))->ns.prop.info
#   list()
#   for(node in ns.prop.info){
#     attrs<-xmlAttrs(node)
#     if("summary" %in% names(attrs)){
#       cat(attrs["summary"],"\n")
#       dt<-cbind(summary=attrs["summary"],data.table(readHTMLTable(node)))
#       print(dt)
#     }
#   }
  tableExtractor<-function(node){
    attrs<-xmlAttrs(node)
    if("summary" %in% names(attrs)){
      cbind(attr=attrs["summary"],
            data.table(readHTMLTable(node, 
              header=c("aspect", "value"))))
    } else {
      data.table()
    }    
  }
  propTables<-lapply(ns.prop.info, tableExtractor)
  propTables<-rbindlist(propTables)
  #setnames(propTables, "Summary","Value", "")
}

extractAllProps<-function(){
  pages<-c("text.html",  "masking.html",  "painting.html", "color.html",
           "interact.html", "filters.html", "pservers.html")
  
  propTables<-lapply(pages, extractOnePropPage)
  rbindlist(propTables)
 
}

extractAllProps()->prop.dt

cleanPropsTable<-function(prop.dt){
  
  prop.dt[,attr:=gsub(" property","",attr) ]
  #prop.dt[,aspect=gsub(":","",aspect)]
  prop.dt[,aspect:=sapply(str_split(aspect,":"), function(x)x[[1]])] 
  prop.dt[,value:=gsub("[‘’]","",value) ]
  prop.dt[,value:=gsub("\\s+"," ",value) ]
  
  
  prop.dt[, aspect:=str_trim(aspect)]  
  prop.dt[, attr:=str_trim(attr)]  
  prop.dt[, value:=str_trim(value)]  
  
  prop.dt
}
 
cleanPropsTable(prop.dt)

#write.table(prop.dt,"test.csv",sep=",",row.names=FALSE,quote=FALSE)
write.table(prop.dt,"test.csv",sep=",",row.names=FALSE,quote=FALSE)
#write(prop.dt, "prop.csv", )
#     #la.dt[, getPresLinkInfo(doc,  attr, loc) ]
#     #lapply(locs, function(loc)getPresLinkInfo(doc, loc))->dts
#     lapply(1:nrow(la.dt), function(i)getPresLinkInfo(doc, la.dt$attr[i], la.dt$loc[i]))->dts
#     #lapply(1:nrow(tmp.df), function(i)paste(tmp.df$x[i],tmp.df$y[i]) )
#     
#     rbindlist(dts)  
#   }
