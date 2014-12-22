#FiltersElement
library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(devtools)
library(assertthat)

# to scour for attributes

source("showMe.R")

#http://www.w3.org/TR/SVG11/
#http://www.w3.org/TR/SVG11/eltindex.html
#grap all page references to svg elements
url="http://www.w3.org/TR/SVG/eltindex.html"
getHTMLLinks(url, relative = TRUE)->tmp.links
unique(gsub("#.*$","", tmp.links))->pages


# for(page in pages2){
#   cat("2: ",page,"\n")
# }
# 
# pageUrl<-"http://www.w3.org/TR/SVG/filters.html"

capitalizeIt<-function(name){
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", name, perl=TRUE)
}

#final.list<-list()

scrape4EleAttrClasses<-function(pageUrl){
  #url<-"http://www.w3.org/TR/SVG/filters.html"
  #url<-"http://www.w3.org/TR/SVG/Overview.html"
  showMe(pageUrl)
  script<-getURL(pageUrl)
  doc <- htmlParse(script)
  #getNodeSet(doc, "//h3/span[@class='element-name']")->ns.eln
  getNodeSet(doc, "//div[@class='element-summary']")->ns.els

  getNodeSet(doc, "//span[@class='expanding']")->ns.expnd
  
  sapply(ns.expnd, function(n)getSibling(n,after=FALSE))->priorNode
  sapply(priorNode, xmlValue)->names.expnd
  
  fn<-function(n){
    vals<-getNodeSet(n,'a')
    vals<-sapply(vals, xmlValue)
    vals<-gsub("[‘’]","",vals)
  }
  
  expnd.list<-lapply(ns.expnd,fn)
  names(expnd.list)<-names.expnd
  
  expnd.list<-expnd.list[!duplicated(expnd.list)] 
#   print(expnd.list)
#   ne<-names(expnd.list)
#   final.list[ne]<-expnd.list[ne]
   sapply(names(expnd.list), function(n)list(data.table(name=n, value=expnd.list[[n]])))->expnd.list
   rbindlist(expnd.list)

}

#scrape4EleAttrClasses(pageUrl)->test.page.dt

elAttrClass.list<-sapply(pages, scrape4EleAttrClasses)

#lapply(pages, scrape4EleAttrClasses)

rbindlist(elAttrClass.list)->eaCS.DT
eaCS.DT<-unique(eaCS.DT)

write.table(eaCS.DT,file="dataTableLink/elementAttrCategorySummary.tsv",
            sep="\t",
            row.names=FALSE,
            quote=FALSE)

