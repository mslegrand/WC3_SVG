#FiltersElement
library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(devtools)
library(assertthat)



url<-"http://www.w3.org/TR/SVG/filters.html"
script<-getURL(url)
doc <- htmlParse(script)
#getNodeSet(doc, "//h3/span[@class='element-name']")->ns.eln
getNodeSet(doc, "//div[@class='element-summary']")->ns.els

node2Vals<-function(node){
  if(length(node)>0){
    vals<-xmlApply(node, xmlValue )
    vals<-unlist(vals, use.names=F)
    vals<-gsub(" — .*$",":",vals)
    vals<-gsub("in’", "in1", vals) #be aware of this kludge
    vals<-gsub("[‘’]","",vals)  
  } else {
    vals<-"NA"
  }
  vals  
}

elementSummaryNode2Table<-function(node){
  ele.name<-xmlValue(node[['div']])
  node.dl<-node[['dl']]
  
  getNodeSet(node.dl, 'dd')->tmp.ns
  val<-xmlValue(tmp.ns[[1]])
  dt1<-data.table(element=ele.name, type='category', value=val)
  
  node.ul<-tmp.ns[[2]][['ul']]
  val<-node2Vals(node.ul)
  dt2<-data.table(element=ele.name, type='content.model', value=val)
  
  node.ul<-tmp.ns[[3]][['ul']]
  val<-node2Vals(node.ul)
  dt3<-data.table(element=ele.name, type='attr', value=val)
  
  dt<-rbindlist(list(dt1,dt2,dt3))
  
}

filter.dt<-rbindlist(lapply( ns.els, elementSummaryNode2Table))

ns.els

# i=5
# node<-ns.els[[i]]
# dt<-elementSummaryNode2Table(node)
#data.table(element=ele.name, type='category', value=category)

# node in ns.els has 2 children: div and dl 
# the element name being describe is given by  xmlValue(node[['div']])
# the category, content model and attributes reside below node[['dl']]
# node[['dl']] has 8 children
# "dt" "dd" "dt" "dd" "dt" "dd" "dt" "dd" 
# we are interested only in the values which reside beneath the dd's
# so setting node2<-node[['dl']] 
# getNodeSet(node2, "dd')->tmp.ns will return thes dd's
# the category is given by 
#  category=xmlValue(tmp.ns[[1]])
# the content model is given by 
#  contentModle=xmlValue(tmp.ns[[2]][['ul']])
# tmp.ns[[3]] has one child tmp.ns[[3]][['ul]]
# tmp.ns[[3]][['ul]] has 8 children all with name li
# these children correspond to 
# core attributes 
# presentation attributes 
# filter primitive attributes 
# ‘class’
# ‘style’
# ‘in’
# ‘type’
# ‘values’

# getNodeSet(doc, "//div[@class='element-summary-name']")->ns.els.name
# getNodeSet(doc, "//div[@class='element-summary']")->ns.els.content
# getNodeSet(doc, "//div[@class='element-summary']")->ns.els.attributes
# 
# 
# #getNodeSet(ns.els[[5]], "/dd")->tmp
# 
# 
# getNodeSet(doc, "//div[@class='element-summary']/dl/dd")->ns.els.stuff
# 
# readHTMLList(ns.els[[5]])
